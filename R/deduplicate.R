# =========================================================
# Deduplication: DOI (primary) + title (fallback)
# =========================================================

# Collapse versioned DOIs: 10.12688/xxx.1, xxx.2, xxx.3 → keep latest
# Applies to F1000Research, Wellcome Open Research, Gates Open Research, etc.
# Pattern: base DOI ending in .N where N is a version number
collapse_versioned_dois <- function(df) {
  if (nrow(df) == 0) return(df)

  # Detect versioned DOIs: pattern like "prefix.N" where N is 1-9
  df <- df |> mutate(
    doi_base = if_else(
      !is.na(doi) & str_detect(doi, "\\.[1-9]\\d?$"),
      str_remove(doi, "\\.[1-9]\\d?$"),
      NA_character_
    ),
    doi_version = if_else(
      !is.na(doi_base),
      as.integer(str_extract(doi, "\\d+$")),
      NA_integer_
    )
  )

  # Split into versioned and non-versioned
  versioned <- df |> filter(!is.na(doi_base))
  non_versioned <- df |> filter(is.na(doi_base))

  if (nrow(versioned) > 0) {
    # Keep only the row with highest version per base DOI
    versioned <- versioned |>
      group_by(doi_base) |>
      slice_max(doi_version, n = 1, with_ties = FALSE) |>
      ungroup()
  }

  bind_rows(non_versioned, versioned) |>
    select(-doi_base, -doi_version)
}

deduplicate_pubs <- function(df) {
  if (nrow(df) == 0) return(df)

  # Remove corrections/errata
  df <- df |> filter(
    !str_detect(str_to_lower(title), "^(correction|erratum|corrigendum)\\b")
  )

  # For versioned DOIs (F1000Research, Wellcome, etc.): keep only the latest version
  # e.g., 10.12688/wellcomeopenres.23033.2 and .3 and .4 → keep .4
  df <- collapse_versioned_dois(df)

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
      authors_full = first(na.omit(authors_full), default = NA_character_),
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
          deduped_doi$authors_full[idx[1]] <- no_doi_matched$authors_full[i]
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
      authors_full = first(na.omit(authors_full), default = NA_character_),
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
