# Local test for large job DuckDB
# Based on examples/duckdb_large_job.R

devtools::load_all()
library(duckdb)
library(DBI)
library(dplyr)

# Setup 5M dataset
con <- dbConnect(duckdb::duckdb(), ":memory:")

dbExecute(con, "
  CREATE TABLE persons AS
  SELECT
    row_number() OVER () AS person_id,
    'Name ' || (random()*100)::INTEGER AS name,
    'Street ' || (random()*100)::INTEGER AS street,
    'Region ' || (random()*5)::INTEGER AS region,
    (date '1950-01-01' + (random() * 18250)::INTEGER) AS birth_date
  FROM generate_series(1, 5000000)
")

strategy <- search_strategy(
  name ~ normalize_text() + word_tokens(),
  region ~ identity(),
  block_by = "region",
  threshold = 0.8
)

# Run test
persons_tbl <- dplyr::tbl(con, "persons")
tokens <- prepare_search_data(
  data = persons_tbl,
  id = "person_id",
  strategy = strategy
)

# Verification
n_tokens <- tokens %>% count() %>% pull(n)
if (n_tokens == 0) stop("Test failed: No tokens created.")
cat("Test passed. Tokens created:", n_tokens, "\n")

dbDisconnect(con)
