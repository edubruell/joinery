# data-raw/generate_street_stopwords.R
# Build joinery::street_stopwords — locative particles & articles that appear
# inside multi-word street names and carry no discriminative signal for
# matching (e.g. German "AN DER ALSTER" -> "ALSTER").
#
# Consumed by normalize_street(drop_stopwords = TRUE), filtered by `lang`.
# Uppercase ASCII so it joins directly against normalize_street()'s already
# uppercased, transliterated tokens.

library(tibble)
library(dplyr)
library(stringi)
library(usethis)

norm_uc <- function(x) {
  x |>
    stringi::stri_trans_general("Any-Latin") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper()
}

# One row per (stopword, lang). Kept deliberately tight: only true locative
# particles and articles, never adjectives ("NEUE", "GROSSE") or directionals
# that can themselves be the discriminating part of a street name.
raw <- tribble(
  ~stopword, ~lang,
  # German — prepositions + articles seen in "An der ...", "Am ...", "Zum ..."
  "an", "de", "am", "de", "in", "de", "im", "de",
  "zum", "de", "zur", "de", "zu", "de",
  "auf", "de", "bei", "de", "beim", "de",
  "vor", "de", "hinter", "de", "unter", "de",
  "der", "de", "die", "de", "das", "de",
  "den", "de", "dem", "de", "des", "de",
  # English
  "the", "en", "of", "en", "at", "en", "on", "en",
  # French
  "de", "fr", "du", "fr", "des", "fr",
  "la", "fr", "le", "fr", "les", "fr", "l", "fr",
  "au", "fr", "aux", "fr",
  # Spanish
  "de", "es", "del", "es",
  "la", "es", "el", "es", "las", "es", "los", "es",
  # Italian
  "di", "it", "del", "it", "della", "it", "delle", "it",
  "dello", "it", "dei", "it", "degli", "it", "da", "it",
  # Portuguese
  "de", "pt", "do", "pt", "da", "pt", "dos", "pt", "das", "pt",
  # Dutch
  "van", "nl", "de", "nl", "den", "nl", "der", "nl",
  "het", "nl", "op", "nl", "aan", "nl"
)

street_stopwords <- raw |>
  transmute(stopword = norm_uc(stopword), lang = lang) |>
  distinct() |>
  arrange(lang, stopword)

usethis::use_data(street_stopwords, overwrite = TRUE)
