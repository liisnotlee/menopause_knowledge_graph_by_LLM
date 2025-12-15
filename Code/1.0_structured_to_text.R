
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(stringr)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="chunniji0909", password="Password")

query_menopause <- "
  SELECT DISTINCT s.nct_id, s.brief_title, s.study_type, c.name AS condition_name
  FROM studies s
  LEFT JOIN conditions c ON s.nct_id = c.nct_id
  WHERE LOWER(c.name) LIKE ANY(ARRAY[
      '%menopause%',
      '%menopausal%',
      '%postmenopause%',
      '%perimenopause%',
      '%climacteric%'
  ])
"
menopause_studies <- dbGetQuery(con, query_menopause)
print(paste0("Total number of studies: ", length(nct_ids)))

# Create a comma-separated list of NCT IDs from your menopause_studies
nct_ids <- unique(menopause_studies$nct_id)
id_list <- paste0("'", paste(nct_ids, collapse = "','"), "'")

# Query outcome analyses + outcomes for each study
query_results <- paste0("
  SELECT
    oa.nct_id,
    o.title AS outcome_title,
    o.time_frame,
    o.description AS outcome_description,
    oa.param_type,
    oa.param_value,
    oa.p_value,
    oa.ci_percent,
    oa.ci_lower_limit,
    oa.ci_upper_limit,
    oa.non_inferiority_type
  FROM outcome_analyses oa
  LEFT JOIN outcomes o
    ON oa.nct_id = o.nct_id AND oa.outcome_id = o.id
  WHERE oa.nct_id IN (", id_list, ")
")
menopause_results <- dbGetQuery(con, query_results)

menopause_sig <- menopause_results %>%
  mutate(p_value = as.numeric(p_value)) %>%        # ensure p_value is numeric
  filter(!is.na(p_value) & p_value < 0.05)

# Create readable text per study
menopause_text <- menopause_sig %>%
  group_by(nct_id, outcome_title, time_frame, outcome_description, non_inferiority_type) %>%
  summarise(
    # Combine all parameter results for the same outcome
    param_summary = paste0(
      param_type, " = ", param_value,
      " (p = ", p_value, ", CI ", ci_lower_limit, "-", ci_upper_limit, ")",
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    result_sentence = str_glue(
      "Outcome: {outcome_title}.",
      "Parameters: {param_summary}. Non-inferiority type: {non_inferiority_type}."
    )
  ) %>%
  group_by(nct_id) %>%
  summarise(
    study_text = paste(unique(result_sentence), collapse = " "),
    .groups = "drop"
  )

query_conclusions <- paste0("
  SELECT
    s.nct_id,
    s.brief_title,
    bs.description AS brief_summary,
    dd.description AS detailed_description
  FROM studies s
  LEFT JOIN brief_summaries bs ON s.nct_id = bs.nct_id
  LEFT JOIN detailed_descriptions dd ON s.nct_id = dd.nct_id
  WHERE s.nct_id IN (", id_list, ")
")

study_info <- dbGetQuery(con, query_conclusions)

# Merge with your textual summaries
menopause_for_llm <- menopause_text %>%
  left_join(study_info, by = "nct_id") %>%
  mutate(
    llm_input = str_glue(
      "Study: {brief_title}\n\nSummary: {brief_summary}\n\nResults: {study_text}"
    )
  )

writeLines(menopause_for_llm$llm_input, "~/Documents/Courses/Project/menopause_knowledge_graph_by_LLM/data/structured_for_llm.txt")
