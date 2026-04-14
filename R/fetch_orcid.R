# =========================================================
# ORCID API Fetching
# =========================================================

fetch_orcid_name <- function(orcid_id) {
  tryCatch({
    res <- GET(
      paste0("https://pub.orcid.org/v3.0/", orcid_id, "/person"),
      add_headers(Accept = "application/json")
    )
    if (status_code(res) != 200) return(NA_character_)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    given <- pluck_chr(data, "name", "given-names", "value")
    family <- pluck_chr(data, "name", "family-name", "value")

    if (!is.na(family) && !is.na(given)) {
      cap <- function(s) paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
      return(paste(cap(given), cap(family)))
    }
    NA_character_
  }, error = function(e) NA_character_)
}

fetch_orcid_works <- function(orcid_id) {
  tryCatch({
    res <- GET(
      paste0("https://pub.orcid.org/v3.0/", orcid_id, "/works"),
      add_headers(Accept = "application/json")
    )
    stop_for_status(res)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    groups <- data$group
    if (is.null(groups) || length(groups) == 0) return(empty_pubs())

    map_dfr(groups, function(g) {
      summary <- g[["work-summary"]][[1]]
      if (is.null(summary)) return(NULL)

      title <- pluck_chr(summary, "title", "title", "value")
      journal <- pluck_chr(summary, "journal-title", "value")
      orcid_type <- pluck_chr(summary, "type") %||% "other"

      pub_date <- summary[["publication-date"]]
      year_val <- as.integer(pluck_chr(pub_date, "year", "value"))
      month_val <- as.integer(pluck_chr(pub_date, "month", "value"))
      if (is.na(year_val)) year_val <- 0L
      if (is.na(month_val)) month_val <- NA_integer_

      ext_ids <- summary[["external-ids"]][["external-id"]]
      doi <- NA_character_
      pmid <- NA_character_
      if (!is.null(ext_ids)) {
        for (eid in ext_ids) {
          if (eid[["external-id-type"]] == "doi") doi <- eid[["external-id-value"]]
          if (eid[["external-id-type"]] == "pmid") pmid <- eid[["external-id-value"]]
        }
      }

      if (!is.na(doi)) doi <- normalize_doi(doi)

      tibble(
        title = title %||% "",
        authors = NA_character_,
        journal = journal %||% "",
        year = year_val,
        month = month_val,
        doi = doi,
        pmid = pmid,
        orcid_type = orcid_type,
        source = "orcid",
        member_id = orcid_id
      )
    })
  }, error = function(e) {
    warning(paste("ORCID fetch failed for", orcid_id, ":", e$message))
    empty_pubs()
  })
}
