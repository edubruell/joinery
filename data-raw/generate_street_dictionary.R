# data-raw/generate_street_dictionary.R
# Build joinery::street_types dataset with safe multilingual normalization

library(tibble)
library(dplyr)
library(stringi)
library(usethis)

# Helper for normalization
norm_lc <- function(x) {
  x |>
    stringi::stri_trans_general("Any-Latin") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower()
}

norm_uc <- function(x) {
  x |>
    stringi::stri_trans_general("Any-Latin") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper()
}


# =====================================================================
# 1. Base dictionary with type = "exact" vs "suffix"
# =====================================================================
# Important:
# - EXACT: abbreviations, short forms, standalone words
# - SUFFIX: only where grammatical forms safely allow suffix matching
# =====================================================================

street_types <- tribble(
  ~canonical, ~variant, ~type, ~lang,
  
  # -------------------------------------------------------------------
  # German (de)
  # -------------------------------------------------------------------
  "STRASSE",  "strasse",  "suffix", "de",
  "STRASSE",  "str",      "suffix", "de",
  "STRASSE",  "str.",     "suffix", "de",
  "STRASSE",  "strasse.", "suffix", "de",
  "STRASSE",  "straße",   "suffix", "de",
  
  "GASSE",    "gasse",    "suffix", "de",
  "GASSE",    "g",        "exact",  "de",
  "GASSE",    "g.",       "exact",  "de",
  
  "WEG",      "weg",      "exact",  "de",
  "PLATZ",    "platz",    "exact",  "de",
  "PLATZ",    "pl",       "exact",  "de",
  "PLATZ",    "pl.",      "exact",  "de",
  "ALLEE",    "allee",    "exact",  "de",
  
  # -------------------------------------------------------------------
  # English (en) — NO suffix matching (prevents LINCOLANE)
  # -------------------------------------------------------------------
  "STREET",    "street",   "exact", "en",
  "STREET",    "st",       "exact", "en",
  "STREET",    "st.",      "exact", "en",
  
  "ROAD",      "road",     "exact", "en",
  "ROAD",      "rd",       "exact", "en",
  "ROAD",      "rd.",      "exact", "en",
  
  "AVENUE",    "avenue",   "exact", "en",
  "AVENUE",    "ave",      "exact", "en",
  "AVENUE",    "ave.",     "exact", "en",
  
  "BOULEVARD", "boulevard","exact", "en",
  "BOULEVARD", "blvd",     "exact", "en",
  "BOULEVARD", "blvd.",    "exact", "en",
  
  "DRIVE",     "drive",    "exact", "en",
  "DRIVE",     "dr",       "exact", "en",
  "DRIVE",     "dr.",      "exact", "en",
  
  "LANE",      "lane",     "exact", "en",
  "LANE",      "ln",       "exact", "en",
  "LANE",      "ln.",      "exact", "en",
  
  # -------------------------------------------------------------------
  # French (fr)
  # - DO NOT allow suffix match for "r." → prevents VICTORUE
  # -------------------------------------------------------------------
  "RUE",        "rue",    "exact", "fr",
  "RUE",        "r",      "exact", "fr",
  "RUE",        "r.",     "exact", "fr",
  
  "AVENUE",     "av",     "exact", "fr",
  "AVENUE",     "av.",    "exact", "fr",
  
  "BOULEVARD",  "bd",     "exact", "fr",
  "BOULEVARD",  "bd.",    "exact", "fr",
  
  "PLACE",      "pl",     "exact", "fr",
  "PLACE",      "pl.",    "exact", "fr",
  
  "QUAI",       "quai",   "exact", "fr",
  "IMPASSE",    "impasse","exact", "fr",
  "IMPASSE",    "imp",    "exact", "fr",
  "IMPASSE",    "imp.",   "exact", "fr",
  
  "COURS",      "cours",  "exact", "fr",
  "CHEMIN",     "chemin", "exact", "fr",
  "CHEMIN",     "ch",     "exact", "fr",
  "CHEMIN",     "ch.",    "exact", "fr",
  "ALLEE",      "allee",  "exact", "fr",
  
  # -------------------------------------------------------------------
  # Spanish (es)
  # -------------------------------------------------------------------
  "CALLE",     "calle",   "exact",  "es",
  "CALLE",     "c",       "exact",  "es",
  "CALLE",     "c.",      "exact",  "es",
  
  "AVENIDA",   "avenida", "exact",  "es",
  "AVENIDA",   "avda",    "exact",  "es",
  "AVENIDA",   "avda.",   "exact",  "es",
  "AVENIDA",   "av",      "exact",  "es",
  "AVENIDA",   "av.",     "exact",  "es",
  
  "PASEO",     "paseo",   "exact",  "es",
  "PASEO",     "po",      "exact",  "es",
  "PASEO",     "po.",     "exact",  "es",
  
  "PLAZA",     "plaza",   "exact",  "es",
  "PLAZA",     "pl",      "exact",  "es",
  "PLAZA",     "pl.",     "exact",  "es",
  
  "CAMINO",    "camino",  "exact",  "es",
  "CAMINO",    "cno",     "exact",  "es",
  "CAMINO",    "cno.",    "exact",  "es",
  
  "CARRERA",   "carrera", "exact",  "es",
  "CARRERA",   "cra",     "exact",  "es",
  "CARRERA",   "cra.",    "exact",  "es",
  
  "RONDA",     "ronda",   "exact",  "es",
  "RONDA",     "rda",     "exact",  "es",
  "RONDA",     "rda.",    "exact",  "es",
  
  # -------------------------------------------------------------------
  # Italian (it)
  # -------------------------------------------------------------------
  "VIA",       "via",     "exact", "it",
  "VIA",       "v",       "exact", "it",
  "VIA",       "v.",      "exact", "it",
  
  "VIALE",     "viale",   "exact", "it",
  "PIAZZA",    "piazza",  "exact", "it",
  "PIAZZA",    "p",       "exact", "it",
  "PIAZZA",    "p.",      "exact", "it",
  
  "CORSO",     "corso",   "exact", "it",
  "CORSO",     "c",       "exact", "it",
  "CORSO",     "c.",      "exact", "it",
  
  "LARGO",     "largo",   "exact", "it",
  "VICOLO",    "vicolo",  "exact", "it",
  "VICOLO",    "vic",     "exact", "it",
  "VICOLO",    "vic.",    "exact", "it",
  
  # -------------------------------------------------------------------
  # Portuguese (pt) — NO suffix matching (prevents CENTRALAMEDA)
  # -------------------------------------------------------------------
  "RUA",       "rua",      "exact", "pt",
  "RUA",       "r",        "exact", "pt",
  "RUA",       "r.",       "exact", "pt",
  
  "AVENIDA",   "avenida",  "exact", "pt",
  "AVENIDA",   "av",       "exact", "pt",
  "AVENIDA",   "av.",      "exact", "pt",
  
  "PRACA",     "praca",    "exact", "pt",
  "PRACA",     "prc",      "exact", "pt",
  "PRACA",     "prc.",     "exact", "pt",
  
  "TRAVESSA",  "travessa", "exact", "pt",
  "TRAVESSA",  "tv",       "exact", "pt",
  "TRAVESSA",  "tv.",      "exact", "pt",
  
  "ALAMEDA",   "alameda",  "exact", "pt",
  "ALAMEDA",   "al",       "exact", "pt",
  "ALAMEDA",   "al.",      "exact", "pt",
  
  # -------------------------------------------------------------------
  # Polish (pl)
  # -------------------------------------------------------------------
  "ULICA",     "ulica",   "exact", "pl",
  "ULICA",     "ul",      "exact", "pl",
  "ULICA",     "ul.",     "exact", "pl",
  
  "ALEJA",     "aleja",   "exact", "pl",
  "ALEJA",     "al",      "exact", "pl",
  "ALEJA",     "al.",     "exact", "pl",
  
  "PLAC",      "plac",    "exact", "pl",
  "PLAC",      "pl",      "exact", "pl",
  "PLAC",      "pl.",     "exact", "pl",
  
  "OSIEDLE",   "osiedle", "exact", "pl",
  "OSIEDLE",   "os",      "exact", "pl",
  "OSIEDLE",   "os.",     "exact", "pl",
  
  # -------------------------------------------------------------------
  # Dutch (nl)
  # -------------------------------------------------------------------
  "STRAAT",    "straat",  "suffix", "nl",
  "STRAAT",    "str",     "exact",  "nl",
  "STRAAT",    "str.",    "exact",  "nl",
  
  "LAAN",      "laan",    "exact", "nl",
  "WEG",       "weg",     "exact", "nl",
  "PLEIN",     "plein",   "exact", "nl",
  "GRACHT",    "gracht",  "exact", "nl",
  "KADE",      "kade",    "exact", "nl",
  
  # -------------------------------------------------------------------
  # Turkish (tr)
  # -------------------------------------------------------------------
  "SOKAK",     "sokak",   "exact", "tr",
  "SOKAK",     "sok",     "exact", "tr",
  "SOKAK",     "sok.",    "exact", "tr",
  
  "CADDE",     "cadde",   "exact", "tr",
  "CADDE",     "cad",     "exact", "tr",
  "CADDE",     "cad.",    "exact", "tr",
  
  "BULVAR",    "bulvar",  "exact", "tr",
  "BULVAR",    "blv",     "exact", "tr",
  "BULVAR",    "blv.",    "exact", "tr",
  
  "MEYDAN",    "meydan",  "exact", "tr",
  "MEYDAN",    "mey",     "exact", "tr",
  "MEYDAN",    "mey.",    "exact", "tr",
  
  # -------------------------------------------------------------------
  # Scandinavian: Swedish + Danish/Norwegian suffix-forms
  # -------------------------------------------------------------------
  # Swedish
  "GATA",      "gata",    "exact",  "sv",
  "GATA",      "gatan",   "suffix", "sv",
  
  "VAGEN",     "vagen",   "exact",  "sv",
  "VAGEN",     "vagen",   "suffix", "sv",
  "VAGEN",     "vagen.",  "suffix", "sv",
  
  "TORG",      "torg",    "exact",  "sv",
  
  # Danish/Norwegian
  "GADE",      "gade",    "exact",  "da",
  "VEJ",       "vej",     "exact",  "da",
  "VEJ",       "v",       "exact",  "da",
  "VEJ",       "v.",      "exact",  "da",
  
  "PLADS",     "plads",   "exact",  "da"
)


# =====================================================================
# 2. Normalize canonical + variant to ASCII + case
# =====================================================================

street_types <- street_types |>
  mutate(
    canonical = norm_uc(canonical),
    variant   = norm_lc(variant)
  ) |>
  distinct()


# =====================================================================
# 3. Save as package dataset
# =====================================================================

usethis::use_data(street_types, overwrite = TRUE)
