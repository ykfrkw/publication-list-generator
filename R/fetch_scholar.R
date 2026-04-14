# =========================================================
# Google Scholar Fetching (via scholar package)
# =========================================================

fetch_scholar_name <- function(scholar_id) {
  tryCatch({
    profile <- scholar::get_profile(scholar_id)
    profile$name
  }, error = function(e) NA_character_)
}

fetch_scholar_works <- function(scholar_id) {
  tryCatch({
    pubs <- scholar::get_publications(scholar_id)
    if (is.null(pubs) || nrow(pubs) == 0) return(empty_pubs())

    pubs |>
      transmute(
        title = as.character(title),
        authors = as.character(author),
        journal = as.character(journal),
        year = as.integer(year),
        month = NA_integer_,
        doi = NA_character_,
        pmid = NA_character_,
        orcid_type = "journal-article",
        source = "scholar",
        member_id = scholar_id
      )
  }, error = function(e) {
    warning(paste("Google Scholar fetch failed for", scholar_id, ":", e$message))
    empty_pubs()
  })
}
