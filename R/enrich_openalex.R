# =========================================================
# OpenAlex Enrichment
# =========================================================

OPENALEX_MAILTO <- "pub-list-generator@example.com"

fetch_openalex_by_doi <- function(doi) {
  tryCatch({
    res <- GET(
      paste0("https://api.openalex.org/works/doi:", doi),
      query = list(mailto = OPENALEX_MAILTO)
    )
    if (status_code(res) != 200) return(NULL)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)

    # Extract authors — keep both full and short
    authorships <- data$authorships %||% list()
    full_names <- map_chr(authorships, function(a) {
      a$author$display_name %||% a$raw_author_name %||% "Unknown"
    })
    short_names <- map_chr(full_names, format_author_short)

    # Extract PMID
    pmid_url <- pluck_chr(data, "ids", "pmid")
    pmid <- if (!is.na(pmid_url)) str_extract(pmid_url, "\\d+$") else NA_character_

    list(
      authors = paste(short_names, collapse = ", "),
      authors_full = paste(full_names, collapse = "|"),
      type = data$type %||% "article",
      journal = pluck_chr(data, "primary_location", "source", "display_name"),
      pmid = pmid,
      year = data$publication_year
    )
  }, error = function(e) NULL)
}

# Format author name when family comes FIRST (Japanese convention): "Kikuchi Sou" → "Kikuchi S"
format_author_family_first <- function(display_name) {
  if (is.na(display_name) || display_name == "") return(display_name)

  cap_word <- function(w) {
    lw <- str_to_lower(w)
    if (lw %in% c("van", "von", "de", "del", "di", "la", "le", "el", "al",
                   "den", "der", "das", "dos", "du", "ten")) return(lw)
    hyph_parts <- str_split(lw, "-")[[1]]
    hyph_parts <- map_chr(hyph_parts, ~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x))))
    paste(hyph_parts, collapse = "-")
  }

  parts <- str_split(str_trim(display_name), "\\s+")[[1]]
  if (length(parts) == 1) return(cap_word(parts[1]))

  # If already in short form (one full word + initial), keep as-is
  stripped <- str_remove(parts, "\\.$")
  is_initial <- nchar(stripped) <= 1 | (nchar(stripped) <= 3 & str_detect(stripped, "^[A-Z]+$"))
  n_full <- sum(!is_initial)
  if (n_full == 1 && any(is_initial)) {
    return(paste(map2_chr(parts, is_initial, function(p, i)
      if (i) toupper(str_remove(p, "\\.$")) else cap_word(p)), collapse = " "))
  }

  parts <- map_chr(parts, cap_word)
  # Family is first, given is rest
  family <- parts[1]
  given_parts <- parts[-1]
  initials <- paste0(substr(given_parts, 1, 1), collapse = "")
  paste(family, initials)
}

# Format author name: "Yuki Furukawa" → "Furukawa Y"
# Handles compound surnames: "Annemieke van Straten" → "van Straten A"
format_author_short <- function(display_name) {
  # Capitalize a word, including after hyphens: "fares-otero" → "Fares-Otero"
  cap_word <- function(w) {
    lw <- str_to_lower(w)
    # Preserve surname particles as lowercase
    if (lw %in% c("van", "von", "de", "del", "di", "la", "le", "el", "al",
                   "den", "der", "das", "dos", "du", "ten")) return(lw)
    hyph_parts <- str_split(lw, "-")[[1]]
    hyph_parts <- map_chr(hyph_parts, ~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x))))
    paste(hyph_parts, collapse = "-")
  }

  parts <- str_split(str_trim(display_name), "\\s+")[[1]]
  if (length(parts) == 1) return(cap_word(parts[1]))

  # Detect if already in short form: "Furukawa Y" or "Schneider-Thoma J"
  # Short form = exactly one multi-char word + one or more single-char/initial parts
  # NOT short form = "Natalia E. Fares-Otero" (multiple full words + middle initial)
  # An "initial" part: single letter, letter with period, or 2-3 uppercase letters (e.g., DD, TA, NE)
  stripped <- str_remove(parts, "\\.$")
  is_initial <- nchar(stripped) <= 1 | (nchar(stripped) <= 3 & str_detect(stripped, "^[A-Z]+$"))
  n_full_words <- sum(!is_initial)
  n_initials <- sum(is_initial)
  if (n_full_words == 1 && n_initials >= 1) {
    # Already short form — capitalize full words, keep initials as uppercase
    result_parts <- map2_chr(parts, is_initial, function(p, is_init) {
      if (is_init) toupper(str_remove(p, "\\.$")) else cap_word(p)
    })
    return(paste(result_parts, collapse = " "))
  }

  parts <- map_chr(parts, cap_word)

  # Detect surname particles
  particles <- c("van", "von", "de", "del", "di", "la", "le", "el", "al",
                  "den", "der", "das", "dos", "du", "ten")
  # Find where surname starts
  surname_start <- length(parts)
  for (k in seq_along(parts)) {
    if (str_to_lower(parts[k]) %in% particles && k < length(parts) && k > 1) {
      surname_start <- k
      break
    }
  }

  family <- paste(parts[surname_start:length(parts)], collapse = " ")
  given_parts <- parts[seq_len(surname_start - 1)]
  initials <- paste0(substr(given_parts, 1, 1), collapse = "")
  paste(family, initials)
}

# Batch enrich publications via OpenAlex
enrich_with_openalex <- function(df, progress_fn = NULL) {
  # Always add the column, even for empty df
  if (!"openalex_type" %in% names(df)) df$openalex_type <- NA_character_
  if (nrow(df) == 0) return(df)

  dois <- which(!is.na(df$doi) & df$doi != "")
  total <- length(dois)

  for (i in seq_along(dois)) {
    idx <- dois[i]
    if (!is.null(progress_fn)) {
      progress_fn(i / total, paste0("Enriching via OpenAlex (", i, "/", total, ")..."))
    }

    meta <- fetch_openalex_by_doi(df$doi[idx])
    if (!is.null(meta)) {
      # Always use OpenAlex authors (standardized short form + full names)
      if (length(meta$authors) > 0 && meta$authors != "") {
        df$authors[idx] <- meta$authors
        df$authors_full[idx] <- meta$authors_full
      }
      if (df$journal[idx] == "" && !is.na(meta$journal)) df$journal[idx] <- meta$journal
      if ((is.na(df$pmid[idx]) || df$pmid[idx] == "") && !is.na(meta$pmid)) df$pmid[idx] <- meta$pmid
      df$openalex_type[idx] <- meta$type
    }

    # Rate limit: ~8 req/sec
    if (i %% 8 == 0 && i < total) Sys.sleep(1)
  }

  df
}

# Title-based OpenAlex search for pubs without DOI
enrich_by_title <- function(df, progress_fn = NULL) {
  no_doi <- which((is.na(df$doi) | df$doi == "") & df$title != "")
  total <- length(no_doi)
  if (total == 0) return(df)

  for (i in seq_along(no_doi)) {
    idx <- no_doi[i]
    if (!is.null(progress_fn)) {
      progress_fn(i / total, paste0("Searching OpenAlex by title (", i, "/", total, ")..."))
    }

    tryCatch({
      res <- GET(
        "https://api.openalex.org/works",
        query = list(
          search = df$title[idx],
          per_page = 1,
          mailto = OPENALEX_MAILTO
        )
      )
      if (status_code(res) == 200) {
        data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
        results <- data$results
        if (length(results) > 0) {
          work <- results[[1]]
          # Verify title match
          oa_title <- str_to_lower(work$title %||% "")
          my_title <- str_to_lower(df$title[idx])
          if (str_remove_all(oa_title, "[^a-z0-9]") == str_remove_all(my_title, "[^a-z0-9]")) {
            doi_raw <- work$doi
            if (!is.null(doi_raw)) df$doi[idx] <- normalize_doi(doi_raw)

            pmid_url <- pluck_chr(work, "ids", "pmid")
            if (!is.na(pmid_url)) df$pmid[idx] <- str_extract(pmid_url, "\\d+$")

            df$openalex_type[idx] <- work$type %||% NA_character_

            if (is.na(df$authors[idx]) || df$authors[idx] == "") {
              full_names <- map_chr(work$authorships %||% list(), function(a) {
                a$author$display_name %||% a$raw_author_name %||% "Unknown"
              })
              df$authors[idx] <- paste(map_chr(full_names, format_author_short), collapse = ", ")
              df$authors_full[idx] <- paste(full_names, collapse = "|")
            }

            if (df$journal[idx] == "") {
              j <- pluck_chr(work, "primary_location", "source", "display_name")
              if (!is.na(j)) df$journal[idx] <- j
            }
          }
        }
      }
    }, error = function(e) NULL)

    if (i %% 5 == 0 && i < total) Sys.sleep(1)
  }

  df
}
