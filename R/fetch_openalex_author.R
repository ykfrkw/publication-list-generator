# =========================================================
# OpenAlex Author Works Fetching
# Replaces Google Scholar — free, CORS-enabled, no scraping
# =========================================================

OPENALEX_AUTHOR_MAILTO <- "pub-list-generator@example.com"

# Fetch author name from OpenAlex author ID
fetch_openalex_author_name <- function(author_id) {
  tryCatch({
    # Strip URL prefix if present
    id <- str_remove(author_id, "^https://openalex.org/")
    res <- GET(
      paste0("https://api.openalex.org/authors/", id),
      query = list(mailto = OPENALEX_AUTHOR_MAILTO)
    )
    if (status_code(res) != 200) return(NA_character_)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    data$display_name %||% NA_character_
  }, error = function(e) NA_character_)
}

# Fetch all works for an OpenAlex author ID
fetch_openalex_author_works <- function(author_id) {
  tryCatch({
    id <- str_remove(author_id, "^https://openalex.org/")
    all_works <- list()
    cursor <- "*"
    page <- 1

    repeat {
      res <- GET(
        "https://api.openalex.org/works",
        query = list(
          filter = paste0("author.id:", id),
          per_page = 200,
          cursor = cursor,
          mailto = OPENALEX_AUTHOR_MAILTO
        )
      )
      if (status_code(res) != 200) break

      data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      results <- data$results
      if (length(results) == 0) break

      all_works <- c(all_works, results)
      cursor <- data$meta$next_cursor
      if (is.null(cursor)) break
      page <- page + 1
      if (page > 20) break  # Safety limit: 4000 works max
      Sys.sleep(0.2)
    }

    if (length(all_works) == 0) return(empty_pubs())

    map_dfr(all_works, function(work) {
      title <- work$title %||% ""
      year <- work$publication_year %||% 0L
      journal <- pluck_chr(work, "primary_location", "source", "display_name")
      oa_type <- work$type %||% "article"

      # DOI
      doi_raw <- work$doi
      doi <- if (!is.null(doi_raw)) normalize_doi(doi_raw) else NA_character_

      # PMID
      pmid_url <- pluck_chr(work, "ids", "pmid")
      pmid <- if (!is.na(pmid_url)) str_extract(pmid_url, "\\d+$") else NA_character_

      # Authors
      authorships <- work$authorships %||% list()
      if (length(authorships) > 0) {
        authors <- paste(map_chr(authorships, function(a) {
          format_author_short(a$author$display_name %||% "Unknown")
        }), collapse = ", ")
      } else {
        authors <- NA_character_
      }

      tibble(
        title = title,
        authors = authors,
        journal = journal %||% "",
        year = as.integer(year),
        month = NA_integer_,
        doi = doi,
        pmid = pmid,
        orcid_type = oa_type,
        source = "openalex",
        member_id = id
      )
    })
  }, error = function(e) {
    warning(paste("OpenAlex author fetch failed for", author_id, ":", e$message))
    empty_pubs()
  })
}

# Resolve ORCID → OpenAlex author ID
resolve_orcid_to_openalex <- function(orcid_id) {
  tryCatch({
    res <- GET(
      "https://api.openalex.org/authors",
      query = list(
        filter = paste0("orcid:https://orcid.org/", orcid_id),
        mailto = OPENALEX_AUTHOR_MAILTO
      )
    )
    if (status_code(res) != 200) return(NA_character_)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    results <- data$results
    if (length(results) > 0) {
      # Return the OpenAlex ID (strip URL)
      str_remove(results[[1]]$id, "^https://openalex.org/")
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}
