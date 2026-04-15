# =========================================================
# Citation Formatting (Vancouver, APA, Harvard, Chicago, Nature)
# =========================================================

format_citation <- function(authors, title, journal, year, doi,
                             style = "Vancouver", bold_names = character(),
                             authors_full = NA_character_) {
  author_str <- format_authors_for_style(authors, authors_full, style, bold_names)
  doi_html <- if (!is.na(doi) && doi != "") {
    paste0('doi: <a href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
  } else ""

  journal_html <- if (!is.na(journal) && journal != "") paste0("<em>", htmltools::htmlEscape(journal), "</em>") else ""

  switch(style,
    "Vancouver" = {
      parts <- c(
        if (author_str != "") paste0(author_str, ".") else NULL,
        paste0(htmltools::htmlEscape(title), "."),
        if (journal_html != "") paste0(journal_html, ".") else NULL,
        if (!is.na(year) && year > 0) paste0(year, ".") else NULL,
        doi_html
      )
      paste(parts, collapse = " ")
    },
    "APA" = {
      parts <- c(
        author_str,
        if (!is.na(year) && year > 0) paste0("(", year, ").") else NULL,
        paste0(htmltools::htmlEscape(title), "."),
        if (journal_html != "") paste0(journal_html, ".") else NULL,
        if (doi_html != "") doi_html else NULL
      )
      paste(parts, collapse = " ")
    },
    "Harvard" = {
      parts <- c(
        author_str,
        if (!is.na(year) && year > 0) paste0("(", year, ")") else NULL,
        paste0("'", htmltools::htmlEscape(title), "',"),
        if (journal_html != "") paste0(journal_html, ".") else NULL,
        doi_html
      )
      paste(parts, collapse = " ")
    },
    "Chicago" = {
      parts <- c(
        if (author_str != "") paste0(author_str, ".") else NULL,
        paste0('"', htmltools::htmlEscape(title), '."'),
        journal_html,
        if (!is.na(year) && year > 0) paste0("(", year, ").") else NULL,
        doi_html
      )
      paste(parts, collapse = " ")
    },
    "Nature" = {
      parts <- c(
        if (author_str != "") paste0(author_str, ".") else NULL,
        paste0(htmltools::htmlEscape(title), "."),
        journal_html,
        if (!is.na(year) && year > 0) paste0("<b>", year, "</b>.") else NULL,
        doi_html
      )
      paste(parts, collapse = " ")
    },
    paste(author_str, title, journal, year, sep = ". ")
  )
}

format_authors_for_style <- function(authors_str, authors_full_str, style, bold_names) {
  if (is.na(authors_str) || authors_str == "") return("")

  # Short-form list for display (comma-separated)
  authors <- str_split(authors_str, ",\\s*")[[1]] |> str_trim()
  if (length(authors) == 0) return("")

  # Full-name list for bold matching (pipe-separated)
  # Fall back to the short form if full is not available
  full_list <- if (!is.na(authors_full_str) && authors_full_str != "") {
    str_split(authors_full_str, "\\|")[[1]] |> str_trim()
  } else {
    authors  # fall back
  }
  # Pad/truncate to match authors length
  if (length(full_list) != length(authors)) {
    # Misalignment: fall back to short form
    full_list <- authors
  }

  # Decide bold status per author using FULL name
  is_bold <- map_lgl(full_list, ~ matches_bold_name(.x, bold_names))

  # Apply markup to the short-form display names
  formatted <- map2_chr(authors, is_bold, function(name, b) {
    if (b) paste0("<strong><u>", htmltools::htmlEscape(name), "</u></strong>")
    else htmltools::htmlEscape(name)
  })
  is_highlighted <- is_bold

  # Truncation: ≤6 show all, >6 show first 3 + hidden highlighted + et al.
  if (length(formatted) > 6) {
    visible <- formatted[1:3]
    rest <- formatted[4:length(formatted)]
    hidden_highlighted <- rest[is_highlighted[4:length(formatted)]]
    if (length(hidden_highlighted) > 0) {
      result <- paste0(
        paste(visible, collapse = ", "),
        ", ...",
        paste(hidden_highlighted, collapse = ", "),
        ", et al."
      )
    } else {
      result <- paste0(paste(visible, collapse = ", "), ", et al.")
    }
  } else {
    result <- switch(style,
      "APA" = {
        if (length(formatted) == 1) formatted[1]
        else paste0(paste(formatted[-length(formatted)], collapse = ", "), ", & ", formatted[length(formatted)])
      },
      "Harvard" = {
        if (length(formatted) == 1) formatted[1]
        else paste0(paste(formatted[-length(formatted)], collapse = ", "), " and ", formatted[length(formatted)])
      },
      "Chicago" = {
        if (length(formatted) == 1) formatted[1]
        else paste0(paste(formatted[-length(formatted)], collapse = ", "), ", and ", formatted[length(formatted)])
      },
      paste(formatted, collapse = ", ")
    )
  }

  result
}

# Check if an author's FULL name matches any bold_name.
# Strategy: all parts of bold_name (normalized) must appear in the author's full name.
# This distinguishes "Yuki Furukawa" from "Yuri Furukawa".
matches_bold_name <- function(full_name, bold_names) {
  if (is.na(full_name) || full_name == "" || length(bold_names) == 0) return(FALSE)

  normalize <- function(s) {
    s |> str_to_lower() |> iconv(to = "ASCII//TRANSLIT") |>
      str_remove_all("[^a-z ]") |> str_squish()
  }

  name_norm <- normalize(full_name)
  if (name_norm == "") return(FALSE)

  # Is the author name itself in short form (exactly one full word + initial(s))?
  # If so, fall back to family + initial strategy.
  author_parts <- str_split(name_norm, "\\s+")[[1]]
  author_is_short <- length(author_parts) >= 2 &&
    sum(nchar(author_parts) >= 2) == 1  # one long + one or more initials

  for (bn in bold_names) {
    bn_norm <- normalize(bn)
    if (bn_norm == "") next
    bn_parts <- str_split(bn_norm, "\\s+")[[1]]

    if (author_is_short) {
      # Author is short-form e.g. "furukawa y" — match against family+initial
      family <- author_parts[nchar(author_parts) >= 2][1]
      author_initials <- paste(author_parts[nchar(author_parts) == 1], collapse = "")
      # bold_name family = last word (or particle-prefixed compound)
      particles <- c("van","von","de","del","di","la","le","el","al","den","der","das","dos")
      surname_start <- length(bn_parts)
      for (k in seq_along(bn_parts)) {
        if (bn_parts[k] %in% particles && k < length(bn_parts) && k > 1) {
          surname_start <- k
          break
        }
      }
      bn_family <- paste(bn_parts[surname_start:length(bn_parts)], collapse = " ")
      bn_initials <- paste0(substr(bn_parts[seq_len(surname_start - 1)], 1, 1), collapse = "")
      if (family == bn_family && (author_initials == "" || str_detect(bn_initials, fixed(author_initials)))) {
        return(TRUE)
      }
    } else {
      # Author is full-form: every part of bold_name must appear in author name
      if (all(vapply(bn_parts, function(p) str_detect(name_norm, fixed(p)), logical(1)))) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# Plain text version (strip HTML)
format_citation_plain <- function(authors, title, journal, year, doi,
                                   style = "Vancouver", bold_names = character(),
                                   authors_full = NA_character_) {
  html <- format_citation(authors, title, journal, year, doi, style, bold_names, authors_full)
  html |>
    str_remove_all("</?strong>") |>
    str_remove_all("</?u>") |>
    str_remove_all("</?em>") |>
    str_remove_all("</?b>") |>
    str_remove_all('<a[^>]*>') |>
    str_remove_all("</a>")
}
