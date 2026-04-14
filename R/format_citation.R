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
  if (name == "" || length(bold_names) == 0) return(htmltools::htmlEscape(name))

  # Normalize for matching: strip accents, lowercase
  normalize_for_match <- function(s) {
    s |>
      str_to_lower() |>
      iconv(to = "ASCII//TRANSLIT") |>  # ü→u, é→e, etc.
      str_remove_all("[^a-z ]") |>
      str_trim()
  }

  name_norm <- normalize_for_match(name)

  for (bn in bold_names) {
    bn_norm <- normalize_for_match(bn)

    # Strategy 1: All parts of bold_name appear in author name (original logic)
    bn_parts <- str_split(bn_norm, "\\s+")[[1]]
    if (length(bn_parts) > 0 && all(vapply(bn_parts, function(p) str_detect(name_norm, fixed(p)), logical(1)))) {
      return(paste0("<strong><u>", htmltools::htmlEscape(name), "</u></strong>"))
    }

    # Strategy 2: Family name + initial match
    # e.g., bold_name="Spyridon Siafis" should match "Siafis S" or "S Siafis"
    # Extract family name (last part, or compound like "van Straten")
    bn_words <- str_split(bn_norm, "\\s+")[[1]]
    if (length(bn_words) >= 2) {
      # Try compound surname detection: if a word is a particle (van, von, de, del, la, le, el, al)
      particles <- c("van", "von", "de", "del", "di", "la", "le", "el", "al", "den", "der", "das", "dos")
      # Find where surname starts (first particle or last word)
      surname_start <- length(bn_words)
      for (k in seq_along(bn_words)) {
        if (bn_words[k] %in% particles && k < length(bn_words)) {
          surname_start <- k
          break
        }
      }
      family <- paste(bn_words[surname_start:length(bn_words)], collapse = " ")
      given_parts <- bn_words[seq_len(surname_start - 1)]
      given_initials <- paste0(substr(given_parts, 1, 1), collapse = "")

      name_words <- str_split(name_norm, "\\s+")[[1]]

      # Check: does name contain family name?
      family_match <- str_detect(name_norm, fixed(family))

      if (family_match) {
        # Check if remaining part is initials of given name
        name_without_family <- str_trim(str_remove(name_norm, fixed(family)))
        name_initials <- paste0(substr(str_split(name_without_family, "\\s*")[[1]], 1, 1), collapse = "")
        # Also try: remaining is just the first letter(s)
        name_remaining_chars <- str_remove_all(name_without_family, "\\s+")

        if (name_remaining_chars == "" ||  # family name only
            str_detect(given_initials, fixed(name_remaining_chars)) ||
            str_detect(name_remaining_chars, fixed(given_initials)) ||
            # Single initial match
            (nchar(name_remaining_chars) == 1 && substr(given_initials, 1, 1) == name_remaining_chars)) {
          return(paste0("<strong><u>", htmltools::htmlEscape(name), "</u></strong>"))
        }
      }
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
