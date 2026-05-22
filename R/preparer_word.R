# ============================================================
# Word-level preparers (text -> text)
# ============================================================
#
# Functions whose signature is character-in / character-out.
# Used as the first stages of a preparer pipeline before tokenization.
# ============================================================

#' Normalize text string
#'
#' This function converts a text string to upper case, transliterates it based on the specified
#' transliteration scheme, retains only alphanumeric characters and spaces, and removes extra spaces.
#'
#' @param text A character string or vector to be normalized.
#' @param transliteration A character string specifying the transliteration scheme to be used,
#'        defaulting to "De-ASCII".
#'
#' @return Returns a normalized, upper-case version of the input text, with non-alphanumeric characters
#'         and extra spaces removed.
#'
#' @examples
#' normalize_text("Cafe Conac")
#' normalize_text("Strasse", transliteration = "Latin-ASCII")
#' @export
#' @import stringi
normalize_text <- function(text, transliteration = "De-ASCII") {
  #Validate inputes to the generate_ngrams function
  c("Input text must be a string" = is.character(text),
    "Input transliteration must be a string" = is.character(transliteration)
  ) |>
    validate_inputs()

  # String to upper case characters
  text <- stri_trans_toupper(text)
  # Transliterate other language specific characters
  text <- stri_trans_general(text, transliteration)
  # Keep only alphanumeric characters and spaces
  text <- stri_replace_all_regex(text, "[^A-Za-z0-9 ]", "")
  # Remove additional spaces if they exist
  text <- stri_trim_both(text)
  text <- stri_replace_all_regex(text, "\\s+", " ")
  return(text)
}

#' Normalize Street Names Across Languages
#'
#' `normalize_street()` standardizes street-type tokens in free-text addresses
#' using a multilingual dictionary of canonical forms. The function performs
#' Unicode normalization, ASCII folding, uppercasing, and whitespace cleanup
#' before replacing known street-type variants.
#'
#' Exact matches (e.g., `"st"`, `"rd."`, `"via"`) are always replaced.
#' Suffix matches (e.g., German `"strasse"` endings or Dutch `"straat"`)
#' are applied **only when `lang` is explicitly specified**, preventing unsafe
#' substitutions such as `"LINCOLANE"` -> `"LINCOLANE"`.
#'
#' @param x A character vector containing street names or address fragments.
#' @param lang Optional language code (e.g., `"de"`, `"en"`, `"fr"`).
#'   When provided, the dictionary is filtered to that language and safe
#'   language-specific suffix matching is enabled.
#' @param dict A dictionary of street-type definitions, typically
#'   [joinery::street_types], containing the columns:
#'   * `canonical` -- canonical uppercase form
#'   * `variant`   -- lowercased normalized variant form
#'   * `type`      -- `"exact"` or `"suffix"`
#'   * `lang`      -- ISO language code
#'
#' @return A character vector of normalized street names. `NA` inputs are
#'   preserved as `NA`.
#'
#' @details
#' Normalization steps include:
#' * Unicode -> Latin transliteration and ASCII folding (`stri_trans_general`)
#' * Conversion to uppercase
#' * Removal of non-alphanumeric characters
#' * Tokenization on spaces and per-token replacement
#'
#' Exact variants are replaced verbatim with their canonical form.
#' Suffix variants are replaced only when:
#' * `lang` is specified, and
#' * the token ends with a known variant suffix for that language.
#'
#' @examples
#' normalize_street("Muellerstrasse", lang = "de")
#' # "MUELLERSTRASSE"
#'
#' normalize_street("123 Main St.")
#' # "123 MAIN STREET"
#'
#' normalize_street("Calle Mayor 3", lang = "es")
#' # "CALLE MAYOR 3"
#'
#' @export
normalize_street <- function(x, lang = NULL, dict = joinery::street_types) {

  # Validate
  c(
    "x must be character" = is.character(x),
    "dict must contain canonical, variant, type, lang" =
      all(c("canonical", "variant", "type", "lang") %in% names(dict))
  ) |> validate_inputs()

  # Separate NA early
  is_na <- is.na(x)

  # Preprocess
  x_norm <- x |>
    stri_trans_general("Any-Latin") |>
    stri_trans_general("Latin-ASCII") |>
    stri_trans_toupper() |>
    stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stri_replace_all_regex("\\s+", " ") |>
    stri_trim_both()

  # Filter dictionary by language
  if (!is.null(lang)) {
    dict <- dict[dict$lang == lang, ]
  }

  # Build exact and suffix lookups
  exact <- dict[dict$type == "exact", ]
  suffix <- dict[dict$type == "suffix", ]

  exact_lookup <- setNames(exact$canonical, exact$variant)
  suffix_lookup <- setNames(suffix$canonical, suffix$variant)
  suffix_variants <- names(suffix_lookup)
  suffix_variants <- suffix_variants[order(stri_length(suffix_variants), decreasing = TRUE)]

  replace_tok <- function(tok) {
    if (is.na(tok)) return(NA_character_)
    tok_l <- tolower(tok)

    # Exact match
    if (tok_l %in% names(exact_lookup))
      return(exact_lookup[[tok_l]])

    # Suffix match (only if lang specified)
    if (!is.null(lang)) {
      ends <- stri_endswith_fixed(tok_l, suffix_variants)
      if (any(ends)) {
        suf <- suffix_variants[which(ends)[1]]
        base <- stri_sub(tok, 1, nchar(tok) - nchar(suf))
        return(paste0(base, suffix_lookup[[suf]]))
      }
    }

    tok
  }

  out <- map_chr(
    stri_split_regex(x_norm, " +"),
    ~ map_chr(.x, replace_tok) |>
      paste(collapse = " ")
  )

  out[is_na] <- NA_character_
  out
}

#' Normalize dates to ISO 8601 format (YYYY-MM-DD)
#'
#' `normalize_date()` parses dates from various formats and standardizes them to
#' ISO 8601 format (YYYY-MM-DD) as a character string. Handles common date
#' formats across locales including European (DD.MM.YYYY), American (MM/DD/YYYY),
#' and ISO-style formats.
#'
#' @param x A character or Date vector containing dates to normalize.
#' @param format Optional format string for parsing (passed to `as.Date()`).
#'   If `NULL` (default), attempts automatic parsing via multiple common formats.
#' @param orders Optional character vector of lubridate order specifications
#'   (e.g., `c("dmy", "mdy", "ymd")`). Used when `format = NULL`.
#'   Defaults to `c("ymd", "dmy", "mdy")`.
#'
#' @return A character vector of dates in ISO 8601 format (YYYY-MM-DD).
#'   Unparseable dates return `NA_character_` with a warning.
#'
#' @details
#' When `format` is provided, uses `as.Date(x, format)` directly.
#' When `format = NULL`, tries `lubridate::parse_date_time()` with the
#' specified `orders` to handle mixed formats flexibly.
#'
#' @examples
#' normalize_date("31.12.2023")
#' # "2023-12-31"
#'
#' normalize_date("12/31/2023")
#' # "2023-12-31"
#'
#' normalize_date(c("2023-01-15", "15.01.2023", "01/15/2023"))
#' # c("2023-01-15", "2023-01-15", "2023-01-15")
#'
#' normalize_date("31-12-2023", format = "%d-%m-%Y")
#' # "2023-12-31"
#'
#' @export
normalize_date <- function(x, format = NULL, orders = c("ymd", "dmy", "mdy")) {

  c(
    "x must be character or Date" = (is.character(x) || inherits(x, "Date")),
    "format must be NULL or character" = (is.null(format) || is.character(format)),
    "orders must be character" = is.character(orders)
  ) |> validate_inputs()

  is_na <- is.na(x)

  if (inherits(x, "Date")) {
    out <- format(x, "%Y-%m-%d")
    out[is_na] <- NA_character_
    return(out)
  }

  if (!is.null(format)) {
    parsed <- as.Date(x, format = format)
    out <- format(parsed, "%Y-%m-%d")
    out[is_na] <- NA_character_

    if (any(is.na(out) & !is_na)) {
      warning("Some dates could not be parsed with the specified format")
    }

    return(out)
  }

  parsed <- lubridate::parse_date_time(x, orders = orders, quiet = TRUE)
  out <- format(parsed, "%Y-%m-%d")
  out[is_na] <- NA_character_

  if (any(is.na(out) & !is_na)) {
    warning("Some dates could not be parsed with orders: ", paste(orders, collapse = ", "))
  }

  out
}


#' Strip vowels from text
#'
#' Removes vowels (A, E, I, O, U) including accented and umlaut variants.
#' Useful for fuzzy matching (e.g. "MUELLER" -> "MLLR", "JOSE" -> "JS").
#'
#' @param text Character vector.
#'
#' @return Character vector with vowels removed.
#'
#' @examples
#' strip_vowels("Mueller")   # "MLLR"
#' strip_vowels("Cafe Noir") # "CF NR"
#' strip_vowels(c("Anna", "Peter"))
#'
#' @export
strip_vowels <- function(text) {
  c("text must be character" = is.character(text)) |>
    validate_inputs()

  if (length(text) == 0L) return(text)

  # Normalize accents -> Latin -> ASCII
  x <- text |>
    stringi::stri_trans_general("Any-Latin") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper()

  # Remove vowels A/E/I/O/U
  # Includes letter Y only if you want it - typical Germanic/Romance matching leaves Y in.
  x <- stringi::stri_replace_all_regex(
    x,
    pattern = "[AEIOU]",
    replacement = ""
  )

  # Trim redundant spaces after deletion
  x <- stringi::stri_trim_both(stringi::stri_replace_all_regex(x, "\\s+", " "))

  x
}


#' Convert Text to Metaphone Encoding
#'
#' This function converts a text string to its Metaphone encoding. The Metaphone algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param text A character string or vector to be converted to Metaphone encoding.
#'
#' @return Returns the Metaphone encoded version of the input text.
#' @examples
#' as_metaphone("Cafe")
#' as_metaphone("Strasse")
#' @export
#' @import phonics
as_metaphone <- function(text) {
  # Normalize accents: Café -> Cafe
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")

  # Handle ß separately (ASCII//TRANSLIT turns it into "ss" on some locales, but not reliably)
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)

  phonics::metaphone(x_norm,clean = FALSE)
}


#' Convert Text to Soundex Encoding
#'
#' This function converts a text string to its Soundex encoding. The Soundex algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param text A character string or vector to be converted to Soundex encoding.
#'
#' @return Returns the Soundex encoded version of the input text.
#' @examples
#' as_soundex("Cafe")
#' as_soundex("Strasse")
#' @export
#' @import phonics
as_soundex <- function(text){
  # Validate inputs to the function
  c("Input text must be a string" = is.character(text)
  ) |>
    validate_inputs()

  # Normalize accents: Café -> Cafe
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")

  # Handle ß separately (ASCII//TRANSLIT turns it into "ss" on some locales, but not reliably)
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)

  phonics::soundex(x_norm,clean = FALSE)
}


#' Convert Text to Cologne Phonetic Encoding
#'
#' This function converts a text string to its Cologne Phonetic encoding. The Cologne Phonetic algorithm
#' is used to encode words phonetically by reducing them to a simplified representation based on their
#' pronunciation, particularly suited for German language.
#'
#' @param text A character string or vector to be converted to Cologne Phonetic encoding.
#'
#' @return Returns the Cologne Phonetic encoded version of the input text.
#' @examples
#' as_cologne("Cafe")
#' as_cologne("Strasse")
#' @export
#' @import phonics
as_cologne <- function(text){
  c("Input text must be a string" = is.character(text)) |>
    validate_inputs()

  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)

  phonics::cologne(x_norm, clean = FALSE)
}


#' Use similarity dictionary to group similar tokens together
#'
#' This function looks up a token in the similarity dictionary and returns the corresponding token group for a token.
#'
#' @param text A character string or vector representing the token to be looked up.
#' @param dict A data table containing the similarity dictionary with tokens and their respective groups.
#'
#' @return Returns the token group corresponding to the input token.
#'
#' @examples
#' dict <- data.table::data.table(
#'   tokens = c("example", "sample"),
#'   token_group = c("example/sample", "example/sample")
#' )
#' use_dictionary("example", dict)
#' use_dictionary("nonexistent", dict)
#' @export
use_dictionary <- function(text, dict) {
  # Validate inputs to the function
  c("Input dict must be a data.table" = data.table::is.data.table(dict)) |>
    validate_inputs()

  lookup_row <- function(r){
    dict[tokens %in% r]$token_group
  }

  map(text,lookup_row)
}
