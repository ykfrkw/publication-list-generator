# =========================================================
# Shared Utilities
# =========================================================

# Safe string extraction
pluck_chr <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_character_)
  if (length(val) == 0) return(NA_character_)
  as.character(val[1])
}

# DOI normalization
normalize_doi <- function(x) {
  x |>
    str_to_lower() |>
    str_trim() |>
    str_remove("^https?://(dx\\.)?doi\\.org/")
}

# ORCID ID normalization
normalize_orcid <- function(x) {
  x |>
    str_trim() |>
    str_remove("^https?://orcid\\.org/") |>
    str_to_upper()
}

# Google Scholar ID normalization
normalize_scholar_id <- function(x) {
  x |>
    str_trim() |>
    str_remove("^https?://scholar\\.google\\.(com|co\\.jp|de|fr|co\\.uk)/citations\\?user=") |>
    str_remove("&.*$")  # strip query params like &hl=ja
}

# researchmap ID normalization
normalize_researchmap_id <- function(x) {
  x |>
    str_trim() |>
    str_remove("^https?://researchmap\\.jp/") |>
    str_remove("/.*$") |>  # strip trailing path segments
    str_remove("/$")
}

# Detect ID type by pattern
is_orcid_id <- function(x) {
  str_detect(x, "\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]")
}

is_scholar_id <- function(x) {
  # Google Scholar IDs are typically 12 chars, alphanumeric + _ + -
  str_detect(x, "^[A-Za-z0-9_-]{10,14}$") & !is_orcid_id(x)
}

is_researchmap_id <- function(x) {
  # researchmap permalinks: alphanumeric + underscores, shorter
  str_detect(x, "^[A-Za-z0-9_]{2,30}$") & !is_orcid_id(x) & !is_scholar_id(x)
}

# Parse member input (Excel/TSV/CSV paste)
parse_member_input <- function(text) {
  if (is.null(text) || str_trim(text) == "") return(tibble())

  lines <- str_split(text, "\n")[[1]] |> str_trim() |> discard(~ .x == "")
  if (length(lines) == 0) return(tibble())

  # Detect delimiter: tab (Excel paste) or comma
  first_line <- lines[1]
  delimiter <- if (str_detect(first_line, "\t")) "\t" else ","

  # Split all lines
  rows <- map(lines, ~ str_split(.x, delimiter)[[1]] |> str_trim())

  # Detect header row
  header_row <- rows[[1]]
  header_lower <- str_to_lower(header_row)

  has_header <- any(str_detect(header_lower, "name|氏名|名前|orcid|scholar|google|researchmap|rm"))

  if (has_header) {
    # Map columns by header keywords
    col_map <- list(name = NA_integer_, orcid = NA_integer_, scholar = NA_integer_, researchmap = NA_integer_)
    for (i in seq_along(header_lower)) {
      h <- header_lower[i]
      if (str_detect(h, "name|氏名|名前")) col_map$name <- i
      else if (str_detect(h, "orcid")) col_map$orcid <- i
      else if (str_detect(h, "scholar|google")) col_map$scholar <- i
      else if (str_detect(h, "researchmap|rm")) col_map$researchmap <- i
    }
    data_rows <- rows[-1]
  } else {
    # Auto-detect columns by content patterns
    col_map <- auto_detect_columns(rows)
    data_rows <- rows
  }

  # Build tibble
  members <- map_dfr(data_rows, function(row) {
    get_col <- function(idx) {
      if (is.na(idx) || idx > length(row)) return(NA_character_)
      val <- str_trim(row[idx])
      if (val == "") return(NA_character_)
      val
    }

    tibble(
      name = get_col(col_map$name),
      orcid = {
        raw <- get_col(col_map$orcid)
        if (is.na(raw)) NA_character_ else normalize_orcid(raw)
      },
      scholar = {
        raw <- get_col(col_map$scholar)
        if (is.na(raw)) NA_character_ else normalize_scholar_id(raw)
      },
      researchmap = {
        raw <- get_col(col_map$researchmap)
        if (is.na(raw)) NA_character_ else normalize_researchmap_id(raw)
      }
    )
  })

  # Filter out completely empty rows
  members |> filter(!is.na(orcid) | !is.na(scholar) | !is.na(researchmap))
}

# Auto-detect column types from content
auto_detect_columns <- function(rows) {
  if (length(rows) == 0) return(list(name = NA, orcid = NA, scholar = NA, researchmap = NA))

  n_cols <- max(map_int(rows, length))
  col_map <- list(name = NA_integer_, orcid = NA_integer_, scholar = NA_integer_, researchmap = NA_integer_)

  for (i in seq_len(n_cols)) {
    vals <- map_chr(rows, ~ if (length(.x) >= i) .x[i] else "")
    non_empty <- vals[vals != ""]
    if (length(non_empty) == 0) next

    # Check if column contains ORCID-like patterns
    if (any(str_detect(non_empty, "\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]|orcid\\.org"))) {
      col_map$orcid <- i
    } else if (any(str_detect(non_empty, "scholar\\.google"))) {
      col_map$scholar <- i
    } else if (any(str_detect(non_empty, "researchmap\\.jp"))) {
      col_map$researchmap <- i
    } else if (all(str_detect(non_empty, "^[A-Za-z0-9_-]{10,14}$"))) {
      # Likely Scholar IDs
      if (is.na(col_map$scholar)) col_map$scholar <- i
    } else if (all(str_detect(non_empty, "^[A-Za-z0-9_]{2,30}$")) && !any(str_detect(non_empty, " "))) {
      # Likely researchmap permalinks
      if (is.na(col_map$researchmap)) col_map$researchmap <- i
    } else {
      # Contains spaces or mixed content → likely names
      if (is.na(col_map$name)) col_map$name <- i
    }
  }

  col_map
}

# Create empty publications tibble
empty_pubs <- function() {
  tibble(
    title = character(),
    authors = character(),
    journal = character(),
    year = integer(),
    month = integer(),
    doi = character(),
    pmid = character(),
    orcid_type = character(),
    source = character(),
    member_id = character()
  )
}
