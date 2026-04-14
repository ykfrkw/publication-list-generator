# =========================================================
# Publication List Generator — Multi-Source Shiny App
# ORCID + OpenAlex + researchmap
# =========================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(httr)
library(jsonlite)
library(glue)
library(htmltools)

# Source all modules
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

LAST_YEAR <- as.integer(format(Sys.Date(), "%Y")) - 1L
MONTH_CHOICES <- setNames(1:12, month.abb)


# =========================================================
# UI
# =========================================================
ui <- page_sidebar(
  title = "Publication List Generator",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 400,
    textAreaInput(
      "members_input",
      "Paste member list",
      placeholder = "https://orcid.org/0000-0003-1317-0220\nhttps://researchmap.jp/yk_frkw",
      rows = 6
    ),

    layout_column_wrap(
      width = 1 / 2,
      numericInput("year_from", "Year From", value = LAST_YEAR, min = 1900, max = 2100),
      selectInput("month_from", "Month From", choices = MONTH_CHOICES, selected = 1)
    ),
    layout_column_wrap(
      width = 1 / 2,
      numericInput("year_to", "Year To", value = LAST_YEAR, min = 1900, max = 2100),
      selectInput("month_to", "Month To", choices = MONTH_CHOICES, selected = 12)
    ),
    selectInput("citation_style", "Citation Style",
      choices = c("Vancouver", "APA", "Harvard", "Chicago", "Nature"),
      selected = "Vancouver"
    ),
    selectInput("sort_by", "Sort By",
      choices = c("Publication Date" = "date", "First Author" = "author"),
      selected = "date"
    ),
    actionButton("generate", "Generate List", class = "btn-primary w-100 btn-lg mt-2")
  ),

  # Main panel
  uiOutput("output_panel"),

  # Disclaimer footer
  tags$footer(
    class = "text-muted small mt-4 pt-3 border-top",
    tags$p(
      tags$b("Disclaimer:"),
      "This tool retrieves publication data from",
      tags$a("ORCID", href = "https://orcid.org", target = "_blank"), ",",
      tags$a("OpenAlex", href = "https://openalex.org", target = "_blank"), ", and",
      tags$a("researchmap", href = "https://researchmap.jp", target = "_blank"), "."
    ),
    tags$ul(
      tags$li("Publication type classification is based on OpenAlex metadata and may not always be accurate."),
      tags$li("Only publications registered in the respective profiles will appear. Missing or incorrect entries in profiles will be reflected."),
      tags$li("OpenAlex author disambiguation may be imperfect. Publications where the member's family name does not appear in the author list are automatically filtered out, but some misattributed works may remain."),
      tags$li("For F1000Research and Wellcome Open Research, peer review approval status is checked via Crossref.")
    )
  ),

)

# =========================================================
# Server
# =========================================================
server <- function(input, output, session) {

  parsed_members <- reactive({
    parse_member_input(input$members_input)
  })

  # Main pipeline
  results <- eventReactive(input$generate, {
    members <- parsed_members()
    req(nrow(members) > 0)

    # Resolve names for all members and merge duplicates
    withProgress(message = "Resolving member names...", value = 0, {
      for (i in seq_len(nrow(members))) {
        incProgress(1 / nrow(members))
        if (is.na(members$name[i]) || members$name[i] == "") {
          # Try ORCID
          if (!is.na(members$orcid[i])) {
            oname <- fetch_orcid_name(members$orcid[i])
            if (!is.na(oname)) { members$name[i] <- oname; next }
          }
          # Try researchmap
          if (!is.na(members$researchmap[i])) {
            rname <- fetch_researchmap_name(members$researchmap[i])
            if (!is.na(rname)) members$name[i] <- rname
          }
        }
      }
    })

    # Merge rows that resolve to the same person (normalized name matching)
    normalize_name <- function(n) {
      if (is.na(n) || n == "") return(NA_character_)
      n |> str_to_lower() |> iconv(to = "ASCII//TRANSLIT") |>
        str_remove_all("[^a-z ]") |> str_squish()
    }
    members$name_key <- map_chr(members$name, normalize_name)

    merged <- members |>
      group_by(name_key) |>
      summarize(
        name = first(na.omit(name)),
        orcid = first(na.omit(orcid), default = NA_character_),
        openalex = first(na.omit(openalex), default = NA_character_),
        researchmap = first(na.omit(researchmap), default = NA_character_),
        .groups = "drop"
      ) |>
      # Preserve original order (by first appearance)
      arrange(match(name_key, unique(members$name_key))) |>
      select(-name_key)

    members <- merged

    all_pubs <- empty_pubs()
    bold_names <- character()
    member_info <- tibble(
      name = character(), orcid = character(),
      openalex = character(), researchmap = character(),
      resolved_name = character()
    )
    errors <- character()

    withProgress(message = "Fetching publications...", value = 0, {
      # Fetch from ORCID + auto-resolve OpenAlex author ID for supplementary coverage
      for (i in seq_len(nrow(members))) {
        orcid_id <- members$orcid[i]
        if (!is.na(orcid_id)) {
          incProgress(0.1, detail = paste0("ORCID: ", orcid_id))
          tryCatch({
            pubs <- fetch_orcid_works(orcid_id)
            all_pubs <- bind_rows(all_pubs, pubs)
            oname <- fetch_orcid_name(orcid_id)
            if (!is.na(oname)) bold_names <- c(bold_names, oname)

            # Auto-resolve ORCID → OpenAlex author ID for supplementary works
            if (is.na(members$openalex[i])) {
              resolved_oa <- resolve_orcid_to_openalex(orcid_id)
              if (!is.na(resolved_oa)) {
                members$openalex[i] <- resolved_oa
              }
            }
          }, error = function(e) {
            errors <<- c(errors, paste("ORCID error:", orcid_id, e$message))
          })
        }
      }

      # Fetch from OpenAlex (author works) — supplements ORCID with additional publications
      for (i in seq_len(nrow(members))) {
        oa_id <- members$openalex[i]
        if (!is.na(oa_id)) {
          incProgress(0.1, detail = paste0("OpenAlex: ", oa_id))
          tryCatch({
            pubs <- fetch_openalex_author_works(oa_id)
            # Filter: only keep pubs where member's family name appears in authors
            member_name <- members$name[i]
            if (!is.na(member_name) && member_name != "") {
              before_count <- nrow(pubs)
              pubs <- filter_openalex_by_name(pubs, member_name)
              filtered_count <- before_count - nrow(pubs)
              if (filtered_count > 0) {
                errors <<- c(errors, paste0(
                  "OpenAlex: Filtered out ", filtered_count,
                  " publications for ", oa_id,
                  " where \"", member_name, "\" was not found in the author list."
                ))
              }
            }
            all_pubs <- bind_rows(all_pubs, pubs)
          }, error = function(e) {
            errors <<- c(errors, paste("OpenAlex error:", oa_id, e$message))
          })
        }
      }

      # Fetch from researchmap
      for (i in seq_len(nrow(members))) {
        rm_id <- members$researchmap[i]
        if (!is.na(rm_id)) {
          incProgress(0.1, detail = paste0("researchmap: ", rm_id))
          tryCatch({
            pubs <- fetch_researchmap_works(rm_id)
            all_pubs <- bind_rows(all_pubs, pubs)
            rname <- fetch_researchmap_name(rm_id)
            if (!is.na(rname)) bold_names <- c(bold_names, rname)
          }, error = function(e) {
            errors <<- c(errors, paste("researchmap error:", rm_id, e$message))
          })
        }
      }

      # Add user-provided display names to bold list
      for (i in seq_len(nrow(members))) {
        if (!is.na(members$name[i]) && members$name[i] != "") {
          bold_names <- c(bold_names, members$name[i])
        }
      }
    })

    bold_names <- unique(bold_names)

    # Filter by year/month range
    year_from <- input$year_from
    month_from <- as.integer(input$month_from)
    year_to <- input$year_to
    month_to <- as.integer(input$month_to)

    if (nrow(all_pubs) > 0) {
      all_pubs <- all_pubs |> mutate(
        month = if_else(is.na(month), 1L, month),
        year_month = year * 100L + month
      ) |> filter(
        year_month >= year_from * 100L + month_from,
        year_month <= year_to * 100L + month_to
      ) |> select(-year_month)
    }

    # Deduplicate
    all_pubs <- deduplicate_pubs(all_pubs)

    # Enrich via OpenAlex
    withProgress(message = "Enriching metadata...", value = 0, {
      all_pubs <- enrich_with_openalex(all_pubs, progress_fn = function(pct, msg) {
        incProgress(pct * 0.6, detail = msg)
      })

      all_pubs <- enrich_by_title(all_pubs, progress_fn = function(pct, msg) {
        incProgress(pct * 0.2, detail = msg)
      })

      incProgress(0.1, detail = "Checking peer review status...")
      all_pubs <- enrich_peer_review(all_pubs)

      incProgress(0.1, detail = "Categorizing...")
      all_pubs <- categorize_all(all_pubs)

      # Remove excluded items (erratum, paratext, corrections)
      all_pubs <- all_pubs |> filter(category != "exclude")
    })

    # Sort
    if (input$sort_by == "author") {
      all_pubs <- all_pubs |> arrange(str_to_lower(authors), desc(year))
    } else {
      all_pubs <- all_pubs |> arrange(desc(year), desc(month), str_to_lower(authors))
    }

    # Build member info — names already resolved in merge step
    for (i in seq_len(nrow(members))) {
      display_name <- if (!is.na(members$name[i]) && members$name[i] != "") members$name[i] else "Unknown"

      if (display_name != "Unknown" && !display_name %in% bold_names) {
        bold_names <- c(bold_names, display_name)
      }

      member_info <- bind_rows(member_info, tibble(
        name = members$name[i] %||% NA_character_,
        orcid = members$orcid[i],
        openalex = members$openalex[i],
        researchmap = members$researchmap[i],
        resolved_name = display_name
      ))
    }

    bold_names <- unique(bold_names)

    list(
      pubs = all_pubs,
      bold_names = bold_names,
      members = member_info,
      errors = errors,
      style = input$citation_style,
      year_from = year_from,
      month_from = month_from,
      year_to = year_to,
      month_to = month_to
    )
  })

  # Render output
  output$output_panel <- renderUI({
    res <- results()
    if (is.null(res)) return(NULL)

    pubs <- res$pubs
    bold_names <- res$bold_names
    members <- res$members
    style <- res$style

    # Title with period
    period_label <- paste0(
      "Publication List (",
      month.abb[res$month_from], " ", res$year_from,
      " \u2013 ",
      month.abb[res$month_to], " ", res$year_to,
      ")"
    )

    # Build member list HTML
    member_html <- if (nrow(members) > 0) {
      member_items <- map(seq_len(nrow(members)), function(i) {
        m <- members[i, ]
        display_name <- if (!is.na(m$resolved_name) && m$resolved_name != "") m$resolved_name else if (!is.na(m$name)) m$name else "Unknown"
        links <- c()
        if (!is.na(m$orcid)) links <- c(links, paste0('<a href="https://orcid.org/', m$orcid, '" target="_blank">ORCID</a>'))
        if (!is.na(m$researchmap)) links <- c(links, paste0('<a href="https://researchmap.jp/', m$researchmap, '" target="_blank">researchmap</a>'))
        link_str <- if (length(links) > 0) paste0(" &mdash; ", paste(links, collapse = " &middot; ")) else ""
        tags$li(HTML(paste0("<b>", htmlEscape(display_name), "</b>", link_str)))
      })
      tags$div(
        tags$p(style = "font-size:16px; font-weight:bold; margin-bottom:8px;", paste0("Members (", nrow(members), ")")),
        tags$ul(class = "list-unstyled mb-3", member_items)
      )
    } else NULL

    # Build categorized publication lists — use <p> with inline style for Word compat
    pub_sections <- map(CATEGORY_ORDER, function(cat) {
      cat_pubs <- pubs |> filter(category == cat)
      if (nrow(cat_pubs) == 0) return(NULL)

      items <- map(seq_len(nrow(cat_pubs)), function(j) {
        p <- cat_pubs[j, ]
        citation_html <- format_citation(p$authors, p$title, p$journal, p$year, p$doi, style, bold_names)
        pmid_html <- if (!is.na(p$pmid) && p$pmid != "") {
          paste0(' PMID: <a href="https://pubmed.ncbi.nlm.nih.gov/', p$pmid, '" target="_blank">', p$pmid, '</a>')
        } else ""
        tags$li(HTML(paste0(citation_html, pmid_html)))
      })

      tags$div(
        class = "mb-3",
        tags$p(style = "font-size:16px; font-weight:bold; margin-bottom:8px;",
               paste0(CATEGORY_LABELS[cat], " (", nrow(cat_pubs), ")")),
        tags$ol(items)
      )
    }) |> discard(is.null)

    error_html <- if (length(res$errors) > 0) {
      tags$div(
        class = "alert alert-warning",
        tags$ul(map(res$errors, tags$li))
      )
    } else NULL

    total <- nrow(pubs)

    # Clipboard JS — use explicit font-size in HTML to avoid Word sizing issues
    copy_js <- tags$script(HTML("
      function copyResults() {
        var el = document.getElementById('results-content');
        if (!el) return;
        var html = '<div style=\"font-family:serif;font-size:12pt;\">' + el.innerHTML + '</div>' +
          '<p style=\"font-size:9pt;color:gray;\">Generated with <a href=\"https://yukifurukawa.jp/publication-list-generator/\">Publication List Generator</a></p>';
        var plain = el.innerText + '\\nGenerated with Publication List Generator (https://yukifurukawa.jp/publication-list-generator/)';
        try {
          navigator.clipboard.write([new ClipboardItem({
            'text/html': new Blob([html], {type: 'text/html'}),
            'text/plain': new Blob([plain], {type: 'text/plain'})
          })]).then(function() {
            var btn = document.getElementById('copy-btn');
            btn.innerText = 'Copied!';
            setTimeout(function() { btn.innerText = 'Copy All'; }, 2000);
          });
        } catch(e) {
          var tmp = document.createElement('div');
          tmp.innerHTML = html;
          tmp.style.position = 'fixed';
          tmp.style.left = '-9999px';
          document.body.appendChild(tmp);
          var range = document.createRange();
          range.selectNodeContents(tmp);
          var sel = window.getSelection();
          sel.removeAllRanges();
          sel.addRange(range);
          document.execCommand('copy');
          sel.removeAllRanges();
          document.body.removeChild(tmp);
          var btn = document.getElementById('copy-btn');
          btn.innerText = 'Copied!';
          setTimeout(function() { btn.innerText = 'Copy All'; }, 2000);
        }
      }
    "))

    tags$div(
      copy_js,
      tags$div(
        class = "d-flex justify-content-between align-items-center mb-3",
        tags$h5(paste0("Results (", total, " publications)")),
        tags$button(
          id = "copy-btn",
          class = "btn btn-outline-secondary btn-sm",
          onclick = "copyResults()",
          "Copy All"
        )
      ),
      error_html,
      tags$div(
        id = "results-content",
        # Title for the period
        tags$p(style = "font-size:18px; font-weight:bold; margin-bottom:12px;", period_label),
        member_html,
        pub_sections
      )
    )
  })
}

# =========================================================
# Run
# =========================================================
shinyApp(ui, server)
