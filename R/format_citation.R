# =========================================================
# Citation Formatting (Vancouver, APA, Harvard, Chicago, Nature)
# =========================================================

format_citation <- function(authors, title, journal, year, doi, style = "Vancouver", bold_names = character()) {
  author_str <- format_authors_for_style(authors, style, bold_names)
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
    # Default
    paste(author_str, title, journal, year, sep = ". ")
  )
}

format_authors_for_style <- function(authors_str, style, bold_names) {
  if (is.na(authors_str) || authors_str == "") return("")

  authors <- str_split(authors_str, ",\\s*")[[1]] |> str_trim()
  if (length(authors) == 0) return("")

  # Apply bold+underline to member names
  formatted <- map_chr(authors, function(name) boldify(name, bold_names))
  is_highlighted <- map_lgl(formatted, ~ str_starts(.x, "<strong>"))

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
      # Vancouver, Nature, default
      paste(formatted, collapse = ", ")
    )
  }

  result
}

boldify <- function(name, bold_names) {
  name_lower <- str_to_lower(name)
  for (bn in bold_names) {
    parts <- str_split(str_to_lower(bn), "\\s+")[[1]]
    if (all(map_lgl(parts, ~ str_detect(name_lower, fixed(.x))))) {
      return(paste0("<strong><u>", htmltools::htmlEscape(name), "</u></strong>"))
    }
  }
  htmltools::htmlEscape(name)
}

# Plain text version (strip HTML)
format_citation_plain <- function(authors, title, journal, year, doi, style = "Vancouver", bold_names = character()) {
  html <- format_citation(authors, title, journal, year, doi, style, bold_names)
  html |>
    str_remove_all("</?strong>") |>
    str_remove_all("</?u>") |>
    str_remove_all("</?em>") |>
    str_remove_all("</?b>") |>
    str_remove_all('<a[^>]*>') |>
    str_remove_all("</a>")
}
