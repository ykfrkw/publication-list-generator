# =========================================================
# researchmap API Fetching
# =========================================================

fetch_researchmap_name <- function(permalink) {
  tryCatch({
    # researchmap doesn't have a separate profile name endpoint in public API,
    # but we can extract from the first paper's author list or use the permalink
    res <- GET(
      paste0("https://api.researchmap.jp/", permalink),
      query = list(format = "json")
    )
    if (status_code(res) != 200) return(NA_character_)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    # Try English name first, then Japanese
    family_en <- pluck_chr(data, "family_name", "en")
    given_en <- pluck_chr(data, "given_name", "en")
    if (!is.na(family_en) && !is.na(given_en)) {
      return(paste(given_en, family_en))
    }

    family_ja <- pluck_chr(data, "family_name", "ja")
    given_ja <- pluck_chr(data, "given_name", "ja")
    if (!is.na(family_ja) && !is.na(given_ja)) {
      return(paste(family_ja, given_ja))
    }

    NA_character_
  }, error = function(e) NA_character_)
}

fetch_researchmap_works <- function(permalink) {
  tryCatch({
    res <- GET(
      paste0("https://api.researchmap.jp/", permalink, "/published_papers"),
      query = list(format = "json", limit = 1000)
    )
    stop_for_status(res)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    items <- data$items
    if (is.null(items) || length(items) == 0) return(empty_pubs())

    map_dfr(items, function(item) {
      title_en <- pluck_chr(item, "paper_title", "en")
      title_ja <- pluck_chr(item, "paper_title", "ja")
      title <- if (!is.na(title_en) && title_en != "") title_en else title_ja

      journal_en <- pluck_chr(item, "publication_name", "en")
      journal_ja <- pluck_chr(item, "publication_name", "ja")
      journal <- if (!is.na(journal_en) && journal_en != "") journal_en else journal_ja

      # Authors
      authors_en <- purrr::pluck(item, "authors", "en", .default = NULL)
      if (!is.null(authors_en)) {
        author_names <- map_chr(authors_en, "name")
        authors <- paste(author_names, collapse = ", ")
      } else {
        authors <- NA_character_
      }

      # Date parsing (YYYY-MM or YYYY)
      pub_date <- pluck_chr(item, "publication_date")
      year_val <- if (!is.na(pub_date)) as.integer(substr(pub_date, 1, 4)) else 0L
      month_val <- if (!is.na(pub_date) && nchar(pub_date) >= 7) as.integer(substr(pub_date, 6, 7)) else NA_integer_

      # DOI & PMID
      doi <- pluck_chr(item, "identifiers", "doi", 1)
      pmid <- pluck_chr(item, "identifiers", "pm_id", 1)
      if (!is.na(doi)) doi <- normalize_doi(doi)

      pub_type <- pluck_chr(item, "published_paper_type") %||% "other"

      tibble(
        title = title %||% "",
        authors = authors,
        journal = journal %||% "",
        year = year_val,
        month = month_val,
        doi = doi,
        pmid = pmid,
        orcid_type = pub_type,
        source = "researchmap",
        member_id = permalink
      )
    })
  }, error = function(e) {
    warning(paste("researchmap fetch failed for", permalink, ":", e$message))
    empty_pubs()
  })
}
