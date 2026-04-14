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

# OpenAlex author ID normalization
normalize_openalex_id <- function(x) {
  x |>
    str_trim() |>
    str_remove("^https?://openalex\\.org/") |>
    str_remove("^authors/")
}

# researchmap ID normalization
normalize_researchmap_id <- function(x) {
  x |>
    str_trim() |>
    str_remove("^https?://researchmap\\.jp/") |>
    str_remove("/.*$") |>
    str_remove("/$")
}

# Detect ID type by pattern
is_orcid_id <- function(x) {
  str_detect(x, "\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]")
}

is_openalex_id <- function(x) {
  # OpenAlex author IDs: A followed by digits (e.g., A5011934579)
  str_detect(x, "^A\\d{5,15}$")
}

is_researchmap_id <- function(x) {
  str_detect(x, "^[A-Za-z0-9_]{2,30}$") & !is_orcid_id(x) & !is_openalex_id(x)
}

# Parse member input (Excel/TSV/CSV paste)
parse_member_input <- function(text) {
  if (is.null(text) || str_trim(text) == "") return(tibble())

  lines <- str_split(text, "\n")[[1]] |> discard(~ str_trim(.x) == "")
  if (length(lines) == 0) return(tibble())

  # Detect delimiter: tab (Excel paste) or comma
  first_line <- lines[1]
  delimiter <- if (str_detect(first_line, "\t")) "\t" else ","

  # Split all lines
  rows <- map(lines, ~ str_split(.x, delimiter)[[1]] |> str_trim())

  # Detect header row
  header_row <- rows[[1]]
  header_lower <- str_to_lower(header_row)

  # Detect header: keywords must be standalone (not inside URLs like "https://orcid.org/...")
  # A header cell is a short keyword, not a URL or ID value
  is_header_cell <- map_lgl(header_row, function(cell) {
    c_lower <- str_to_lower(str_trim(cell))
    # Must be a short keyword, not a URL or ID
    if (str_detect(c_lower, "^https?://")) return(FALSE)
    if (str_detect(c_lower, "^\\d{4}-\\d{4}")) return(FALSE)  # ORCID ID
    if (str_detect(c_lower, "^a\\d{5,}$")) return(FALSE)  # OpenAlex ID
    str_detect(c_lower, "^(name|氏名|名前|orcid|openalex|researchmap|rm)$")
  })
  has_header <- any(is_header_cell)

  if (has_header) {
    col_map <- list(name = NA_integer_, orcid = NA_integer_, openalex = NA_integer_, researchmap = NA_integer_)
    for (i in seq_along(header_lower)) {
      h <- header_lower[i]
      if (str_detect(h, "name|氏名|名前")) col_map$name <- i
      else if (str_detect(h, "orcid")) col_map$orcid <- i
      else if (str_detect(h, "openalex")) col_map$openalex <- i
      else if (str_detect(h, "researchmap|rm")) col_map$researchmap <- i
    }
    data_rows <- rows[-1]
  } else {
    col_map <- auto_detect_columns(rows)
    data_rows <- rows
  }

  members <- map_dfr(data_rows, function(row) {
    get_col <- function(idx) {
      if (is.na(idx) || idx > length(row)) return(NA_character_)
      val <- str_trim(row[idx])
      if (val == "") return(NA_character_)
      val
    }

    # Get raw values
    raw_name <- get_col(col_map$name)
    raw_orcid <- get_col(col_map$orcid)
    raw_openalex <- get_col(col_map$openalex)
    raw_researchmap <- get_col(col_map$researchmap)

    # Also scan ALL cells for URLs that belong in specific columns
    # This handles cases where a URL is pasted into the wrong column or a single-column input
    all_vals <- map_chr(seq_along(row), ~ str_trim(row[.x]))
    for (v in all_vals) {
      if (is.na(v) || v == "") next
      if (str_detect(v, "orcid\\.org") && is.na(raw_orcid)) raw_orcid <- v
      if (str_detect(v, "openalex\\.org") && is.na(raw_openalex)) raw_openalex <- v
      if (str_detect(v, "researchmap\\.jp") && is.na(raw_researchmap)) raw_researchmap <- v
      # Detect bare ORCID pattern in any cell
      if (is.na(raw_orcid) && str_detect(v, "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]$")) raw_orcid <- v
      # Detect bare OpenAlex ID
      if (is.na(raw_openalex) && str_detect(v, "^A\\d{5,15}$")) raw_openalex <- v
    }

    tibble(
      name = raw_name,
      orcid = if (is.na(raw_orcid)) NA_character_ else normalize_orcid(raw_orcid),
      openalex = if (is.na(raw_openalex)) NA_character_ else normalize_openalex_id(raw_openalex),
      researchmap = if (is.na(raw_researchmap)) NA_character_ else normalize_researchmap_id(raw_researchmap)
    )
  })

  members |> filter(!is.na(orcid) | !is.na(openalex) | !is.na(researchmap))
}

# Auto-detect column types from content
auto_detect_columns <- function(rows) {
  if (length(rows) == 0) return(list(name = NA, orcid = NA, openalex = NA, researchmap = NA))

  n_cols <- max(map_int(rows, length))
  col_map <- list(name = NA_integer_, orcid = NA_integer_, openalex = NA_integer_, researchmap = NA_integer_)

  for (i in seq_len(n_cols)) {
    vals <- map_chr(rows, ~ if (length(.x) >= i) .x[i] else "")
    non_empty <- vals[vals != ""]
    if (length(non_empty) == 0) next

    if (any(str_detect(non_empty, "\\d{4}-\\d{4}-\\d{4}-\\d{3}[\\dX]|orcid\\.org"))) {
      col_map$orcid <- i
    } else if (any(str_detect(non_empty, "openalex\\.org|^A\\d{5,15}$"))) {
      col_map$openalex <- i
    } else if (any(str_detect(non_empty, "researchmap\\.jp"))) {
      col_map$researchmap <- i
    } else if (all(str_detect(non_empty, "^[A-Za-z0-9_]{2,30}$")) && !any(str_detect(non_empty, " "))) {
      if (is.na(col_map$researchmap)) col_map$researchmap <- i
    } else {
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
