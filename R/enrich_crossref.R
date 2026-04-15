# =========================================================
# Crossref Peer Review Check (F1000Research, Wellcome, etc.)
# =========================================================

CROSSREF_MAILTO <- "pub-list-generator@example.com"

check_peer_review_approval <- function(doi) {
  tryCatch({
    res <- GET(
      paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE)),
      query = list(mailto = CROSSREF_MAILTO)
    )
    if (status_code(res) != 200) return(FALSE)

    data <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    assertions <- data$message$assertion %||% list()

    for (a in assertions) {
      name <- a$name %||% ""
      value <- str_to_lower(a$value %||% "")

      if (name == "referee-status" && (value == "indexed" || str_detect(value, "approved"))) {
        return(TRUE)
      }
      if (str_starts(name, "referee-response") && str_detect(value, "approved")) {
        return(TRUE)
      }
    }

    FALSE
  }, error = function(e) FALSE)
}

# Apply peer review check to open-review journal publications
enrich_peer_review <- function(df) {
  if (!"peer_review_approved" %in% names(df)) df$peer_review_approved <- NA
  if (nrow(df) == 0) return(df)

  open_review_idx <- which(!is.na(df$doi) & vapply(df$journal, is_open_review_journal, logical(1)))

  for (idx in open_review_idx) {
    df$peer_review_approved[idx] <- check_peer_review_approval(df$doi[idx])
    Sys.sleep(0.4)
  }

  df
}
