# =========================================================
# Publication Type Categorization
# =========================================================

PREPRINT_SERVERS <- c(
  "medrxiv", "biorxiv", "arxiv", "ssrn", "chemrxiv", "psyarxiv",
  "preprints.org", "research square", "authorea"
)

OPEN_REVIEW_JOURNALS <- c(
  "f1000research", "f1000 research", "wellcome open research",
  "gates open research", "hrb open research"
)

is_preprint_server <- function(journal) {
  j <- str_to_lower(journal)
  any(str_detect(j, fixed(PREPRINT_SERVERS)))
}

is_open_review_journal <- function(journal) {
  j <- str_to_lower(journal)
  any(map_lgl(OPEN_REVIEW_JOURNALS, ~ str_detect(j, fixed(.x))))
}

# Vectorized version for use in mutate
categorize_pub <- function(journal, orcid_type, openalex_type, peer_review_approved) {
  journal <- journal %||% ""
  oa_type <- str_to_lower(openalex_type %||% "")
  orcid_t <- str_to_lower(orcid_type %||% "")

  # 1. Preprint servers
  if (is_preprint_server(journal)) return("preprint")
  if (orcid_t == "preprint" && !is_open_review_journal(journal)) return("preprint")

  # 2. Open peer-review journals
  if (is_open_review_journal(journal)) {
    return(if (isTRUE(peer_review_approved)) "original" else "preprint")
  }

  # 3. OpenAlex type
  if (oa_type != "" && !is.na(oa_type)) {
    if (oa_type == "letter") return("letter")
    if (oa_type == "editorial") return("editorial")
    if (oa_type %in% c("article", "review")) return("original")
    if (oa_type == "preprint") return("preprint")
    if (oa_type %in% c("erratum", "paratext")) return("other")
  }

  # 4. Fallback to ORCID/researchmap type
  if (orcid_t %in% c("journal-article", "review", "scientific_journal")) return("original")
  if (str_detect(orcid_t, "letter")) return("letter")
  if (str_detect(orcid_t, "editorial|comment")) return("editorial")
  if (str_detect(orcid_t, "book|chapter|conference|abstract|report|dissertation|working-paper|other")) return("other")

  "original"
}

# Apply categorization to dataframe
categorize_all <- function(df) {
  df |> mutate(
    category = pmap_chr(
      list(journal, orcid_type, openalex_type, peer_review_approved),
      categorize_pub
    )
  )
}

CATEGORY_ORDER <- c("original", "preprint", "letter", "editorial", "other")
CATEGORY_LABELS <- c(
  original = "Original Articles & Reviews",
  preprint = "Preprints",
  letter = "Letters",
  editorial = "Editorials",
  other = "Other Publication Types"
)
