# =========================================================
# Deduplication: DOI (primary) + title (fallback)
# =========================================================

deduplicate_pubs <- function(df) {
  if (nrow(df) == 0) return(df)

  # Normalize title for matching
  df <- df |> mutate(
    title_key = str_to_lower(title) |> str_remove_all("[^a-z0-9]")
  )

  # Phase 1: group by DOI (for rows that have DOI)
  has_doi <- df |> filter(!is.na(doi) & doi != "")
  no_doi <- df |> filter(is.na(doi) | doi == "")

  deduped_doi <- has_doi |>
    group_by(doi) |>
    summarize(
      title = first(title),
      authors = first(na.omit(authors), default = NA_character_),
      journal = first(na.omit(journal[journal != ""]), default = ""),
      year = first(year),
      month = first(na.omit(month), default = NA_integer_),
      pmid = first(na.omit(pmid), default = NA_character_),
      orcid_type = first(orcid_type),
      source = paste(unique(source), collapse = ","),
      member_id = paste(unique(member_id), collapse = ","),
      title_key = first(title_key),
      .groups = "drop"
    )

  # Phase 2: match remaining no-DOI pubs against DOI pubs by title
  matched_keys <- deduped_doi$title_key
  no_doi_new <- no_doi |> filter(!title_key %in% matched_keys)

  # For no-DOI pubs that match, merge their member_ids into existing rows
  no_doi_matched <- no_doi |> filter(title_key %in% matched_keys)
  if (nrow(no_doi_matched) > 0) {
    for (i in seq_len(nrow(no_doi_matched))) {
      idx <- which(deduped_doi$title_key == no_doi_matched$title_key[i])
      if (length(idx) > 0) {
        existing_members <- str_split(deduped_doi$member_id[idx[1]], ",")[[1]]
        new_member <- no_doi_matched$member_id[i]
        if (!new_member %in% existing_members) {
          deduped_doi$member_id[idx[1]] <- paste(c(existing_members, new_member), collapse = ",")
        }
        existing_sources <- str_split(deduped_doi$source[idx[1]], ",")[[1]]
        new_source <- no_doi_matched$source[i]
        if (!new_source %in% existing_sources) {
          deduped_doi$source[idx[1]] <- paste(c(existing_sources, new_source), collapse = ",")
        }
        # Fill missing authors
        if (is.na(deduped_doi$authors[idx[1]]) && !is.na(no_doi_matched$authors[i])) {
          deduped_doi$authors[idx[1]] <- no_doi_matched$authors[i]
        }
      }
    }
  }

  # Phase 3: deduplicate remaining no-DOI by title
  deduped_no_doi <- no_doi_new |>
    group_by(title_key) |>
    summarize(
      title = first(title),
      authors = first(na.omit(authors), default = NA_character_),
      journal = first(na.omit(journal[journal != ""]), default = ""),
      year = first(year),
      month = first(na.omit(month), default = NA_integer_),
      doi = first(na.omit(doi), default = NA_character_),
      pmid = first(na.omit(pmid), default = NA_character_),
      orcid_type = first(orcid_type),
      source = paste(unique(source), collapse = ","),
      member_id = paste(unique(member_id), collapse = ","),
      .groups = "drop"
    )

  bind_rows(deduped_doi, deduped_no_doi) |>
    select(-title_key)
}
