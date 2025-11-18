## data-raw/generate_street_dictionary.R
## Build joinery::street_types package dataset

library(tibble)
library(dplyr)
library(stringi)
library(usethis)

# -------------------------------------------------------------------
# 1. Define dictionary using tribble
# -------------------------------------------------------------------

street_types <- tribble(
  ~canonical, ~variant,
  # German
  "STRASSE", "strasse",
  "STRASSE", "straße",
  "STRASSE", "str",
  "STRASSE", "str.",
  # English
  "STREET",  "street",
  "STREET",  "st",
  "STREET",  "st.",
  "ROAD",    "road",
  "ROAD",    "rd",
  "ROAD",    "rd.",
  "AVENUE",  "avenue",
  "AVENUE",  "ave",
  "AVENUE",  "ave.",
  # French
  "RUE",     "rue",
  # Spanish
  "CALLE",   "calle",
  # Polish
  "ULICA",   "ul",
  # Turkish
  "SOKAK",   "sok",
  "CADDE",   "cad"
)

# -------------------------------------------------------------------
# 2. Normalize canonical + variant
# -------------------------------------------------------------------

street_types <- street_types %>%
  mutate(
    canonical = canonical |>
      stringi::stri_trans_general("Any-Latin") |>
      stringi::stri_trans_general("Latin-ASCII") |>
      toupper(),
    
    variant = variant |>
      stringi::stri_trans_general("Any-Latin") |>
      stringi::stri_trans_general("Latin-ASCII") |>
      tolower()
  ) %>%
  distinct()

# -------------------------------------------------------------------
# 3. Save as package data
# -------------------------------------------------------------------

usethis::use_data(street_types, overwrite = TRUE)

message("street_types.rda created successfully.")
