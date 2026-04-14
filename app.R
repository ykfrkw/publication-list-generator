# =========================================================
# Publication List Generator — Multi-Source Shiny App
# ORCID + Google Scholar + researchmap
# =========================================================

library(shiny)
library(bslib)
library(tidyverse)
library(httr)
library(jsonlite)
library(scholar)
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
    width = 380,
    textAreaInput(
      "members_input",
      tags$span(
        "Paste member list ",
        tags$small("(Excel/TSV/CSV)", class = "text-muted")
      ),
      placeholder = "Name\tORCID\tScholar\tresearchmap\nYuki Furukawa\t0000-0003-1317-0220\t_twsS0cAAAAJ\tyk_frkw\nStefan Leucht\t0000-0002-4573-7732",
      rows = 6
    ),
    tableOutput("members_preview"),

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
      tags$a("Google Scholar", href = "https://scholar.google.com", target = "_blank"), ", and",
      tags$a("researchmap", href = "https://researchmap.jp", target = "_blank"), ",",
      "and enriches it using",
      tags$a("OpenAlex", href = "https://openalex.org", target = "_blank"), "."
    ),
    tags$ul(
      tags$li("Publication type classification is based on OpenAlex metadata and may not always be accurate."),
      tags$li("Only publications registered in the respective profiles will appear. Missing or incorrect entries in profiles will be reflected."),
      tags$li("For F1000Research and Wellcome Open Research, peer review approval status is checked via Crossref.")
    )
  )
)

# =========================================================
# Server
# =========================================================
server <- function(input, output, session) {

  # Reactive: parsed members from input
  parsed_members <- reactive({
    parse_member_input(input$members_input)
  })

  # Preview table
  output$members_preview <- renderTable({
    m <- parsed_members()
    if (nrow(m) == 0) return(NULL)

    m |> mutate(
      ORCID = if_else(!is.na(orcid), "\u2713", ""),
      Scholar = if_else(!is.na(scholar), "\u2713", ""),
      researchmap = if_else(!is.na(researchmap), "\u2713", ""),
      Name = if_else(is.na(name), "\u2014", name)
    ) |> select(Name, ORCID, Scholar, researchmap)
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

  # Main pipeline
  results <- eventReactive(input$generate, {
    members <- parsed_members()
    req(nrow(members) > 0)

    all_pubs <- empty_pubs()
    bold_names <- character()
    member_info <- tibble(
      name = character(), orcid = character(),
      scholar = character(), researchmap = character(),
      resolved_name = character()
    )
    errors <- character()

    n_total <- sum(!is.na(members$orcid)) + sum(!is.na(members$scholar)) + sum(!is.na(members$researchmap))
    step <- 0

    withProgress(message = "Fetching publications...", value = 0, {
      # Fetch from ORCID
      for (i in seq_len(nrow(members))) {
        orcid_id <- members$orcid[i]
        if (!is.na(orcid_id)) {
          step <- step + 1
          incProgress(1 / max(n_total, 1), detail = paste0("ORCID: ", orcid_id))
          tryCatch({
            pubs <- fetch_orcid_works(orcid_id)
            all_pubs <- bind_rows(all_pubs, pubs)
            oname <- fetch_orcid_name(orcid_id)
            if (!is.na(oname)) bold_names <- c(bold_names, oname)
          }, error = function(e) {
            errors <<- c(errors, paste("ORCID error:", orcid_id, e$message))
          })
        }
      }

      # Fetch from Google Scholar
      for (i in seq_len(nrow(members))) {
        scholar_id <- members$scholar[i]
        if (!is.na(scholar_id)) {
          step <- step + 1
          incProgress(1 / max(n_total, 1), detail = paste0("Scholar: ", scholar_id))
          tryCatch(
            withCallingHandlers({
              pubs <- fetch_scholar_works(scholar_id)
              all_pubs <- bind_rows(all_pubs, pubs)
              sname <- fetch_scholar_name(scholar_id)
              if (!is.na(sname)) bold_names <- c(bold_names, sname)
              if (nrow(pubs) == 0) {
                errors <<- c(errors, paste0("Google Scholar: Could not fetch data for ", scholar_id, ". Google may be blocking requests."))
              }
            }, warning = function(w) invokeRestart("muffleWarning")),
            error = function(e) {
              errors <<- c(errors, paste("Scholar error:", scholar_id, e$message))
            }
          )
        }
      }

      # Fetch from researchmap
      for (i in seq_len(nrow(members))) {
        rm_id <- members$researchmap[i]
        if (!is.na(rm_id)) {
          step <- step + 1
          incProgress(1 / max(n_total, 1), detail = paste0("researchmap: ", rm_id))
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

      # Title-based search for pubs without DOI
      all_pubs <- enrich_by_title(all_pubs, progress_fn = function(pct, msg) {
        incProgress(pct * 0.2, detail = msg)
      })

      # Peer review check
      incProgress(0.1, detail = "Checking peer review status...")
      all_pubs <- enrich_peer_review(all_pubs)

      # Categorize
      incProgress(0.1, detail = "Categorizing...")
      all_pubs <- categorize_all(all_pubs)
    })

    # Sort
    if (input$sort_by == "author") {
      all_pubs <- all_pubs |> arrange(str_to_lower(authors), desc(year))
    } else {
      all_pubs <- all_pubs |> arrange(desc(year), desc(month), str_to_lower(authors))
    }

    # Build member info (preserve input order)
    for (i in seq_len(nrow(members))) {
      resolved <- NA_character_
      # Try to find a resolved name
      for (bn in bold_names) {
        if (!is.na(members$name[i]) && str_detect(str_to_lower(bn), fixed(str_to_lower(members$name[i])))) {
          resolved <- bn
          break
        }
      }
      member_info <- bind_rows(member_info, tibble(
        name = members$name[i] %||% NA_character_,
        orcid = members$orcid[i],
        scholar = members$scholar[i],
        researchmap = members$researchmap[i],
        resolved_name = if (is.na(resolved)) bold_names[i] %||% members$name[i] else resolved
      ))
    }

    list(
      pubs = all_pubs,
      bold_names = bold_names,
      members = member_info,
      errors = errors,
      style = input$citation_style
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

    # Build member list HTML
    member_html <- if (nrow(members) > 0) {
      member_items <- map(seq_len(nrow(members)), function(i) {
        m <- members[i, ]
        display_name <- if (!is.na(m$resolved_name)) m$resolved_name else if (!is.na(m$name)) m$name else "Unknown"
        links <- c()
        if (!is.na(m$orcid)) links <- c(links, paste0('<a href="https://orcid.org/', m$orcid, '" target="_blank">ORCID</a>'))
        if (!is.na(m$scholar)) links <- c(links, paste0('<a href="https://scholar.google.com/citations?user=', m$scholar, '" target="_blank">Google Scholar</a>'))
        if (!is.na(m$researchmap)) links <- c(links, paste0('<a href="https://researchmap.jp/', m$researchmap, '" target="_blank">researchmap</a>'))
        tags$li(HTML(paste0("<b>", htmlEscape(display_name), "</b> &mdash; ", paste(links, collapse = " &middot; "))))
      })
      tags$div(
        tags$h6(class = "mb-2", paste0("Members (", nrow(members), ")")),
        tags$ul(class = "list-unstyled mb-3", member_items)
      )
    } else NULL

    # Build categorized publication lists
    pub_sections <- map(CATEGORY_ORDER, function(cat) {
      cat_pubs <- pubs |> filter(category == cat)
      if (nrow(cat_pubs) == 0) return(NULL)

      items <- map(seq_len(nrow(cat_pubs)), function(j) {
        p <- cat_pubs[j, ]
        citation_html <- format_citation(p$authors, p$title, p$journal, p$year, p$doi, style, bold_names)
        pmid_html <- if (!is.na(p$pmid) && p$pmid != "") {
          paste0(' <small class="text-muted">PMID: <a href="https://pubmed.ncbi.nlm.nih.gov/', p$pmid, '" target="_blank">', p$pmid, '</a></small>')
        } else ""
        tags$li(HTML(paste0(citation_html, pmid_html)))
      })

      tags$div(
        class = "mb-3",
        tags$h6(class = "mb-2", paste0(CATEGORY_LABELS[cat], " (", nrow(cat_pubs), ")")),
        tags$ol(items)
      )
    }) |> discard(is.null)

    # Error display
    error_html <- if (length(res$errors) > 0) {
      tags$div(
        class = "alert alert-warning",
        tags$ul(map(res$errors, tags$li))
      )
    } else NULL

    total <- nrow(pubs)

    # Clipboard JS
    copy_js <- tags$script(HTML(sprintf("
      function copyResults() {
        var el = document.getElementById('results-content');
        if (!el) return;
        var html = el.innerHTML + '<p style=\"font-size:small;color:gray;\">Generated with <a href=\"https://yukifurukawa.jp/publication-list-generator/\">Publication List Generator</a></p>';
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
          // Fallback
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
    ")))

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
