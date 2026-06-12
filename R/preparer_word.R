# ============================================================
# Word-level preparers (text -> text)
# ============================================================
#
# Functions whose signature is character-in / character-out.
# Used as the first stages of a preparer pipeline before tokenization.
# ============================================================

#' Normalize text for matching
#'
#' The usual first step in a preparer pipeline. Folds text to upper case,
#' transliterates accented and non-Latin characters to ASCII, drops anything
#' that is not a letter, digit, or space, and collapses runs of whitespace. The
#' point is to make superficial differences in case, accents, and punctuation
#' disappear so that `"Cafe-Conac"` and `"cafe conac"` reduce to the same text
#' before it is split into tokens.
#'
#' Returns text, so it goes ahead of a token generator such as [word_tokens()]
#' in a strategy: `name ~ normalize_text() + word_tokens()`.
#'
#' @param text A character string or vector to normalize.
#' @param transliteration A transliteration scheme passed to
#'   [stringi::stri_trans_general()], defaulting to `"De-ASCII"` (German-aware
#'   folding, which expands umlauts to digraphs such as `ue` and `oe`). Use
#'   `"Latin-ASCII"` for plain accent stripping, which drops the diacritic
#'   instead of expanding it.
#'
#' @return A character vector the same length as `text`: upper-cased, ASCII,
#'   alphanumeric-and-space only, with surrounding and repeated spaces removed.
#'
#' @examples
#' normalize_text("Cafe Conac")
#' normalize_text("Strasse", transliteration = "Latin-ASCII")
#'
#' @family text normalizers
#' @seealso [word_tokens()], the token generator that usually follows.
#' @export
#' @import stringi
normalize_text <- function(text, transliteration = "De-ASCII") {
  check_character(text)
  check_string(transliteration)

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

#' Normalize street names across languages
#'
#' Street names are written many ways for the same place: `"Hauptstr."`,
#' `"Hauptstrasse"`, `"Haupt Strasse"`. `normalize_street()` collapses those
#' variants to one canonical spelling so an address column matches on the street
#' name rather than on its abbreviation. It normalizes Unicode, folds to ASCII,
#' upper-cases, and cleans whitespace, then rewrites known street-type tokens
#' from a multilingual dictionary.
#'
#' Returns text, so it sits where [normalize_text()] would in a pipeline, ahead
#' of a token generator: `street ~ normalize_street(lang = "de") + word_tokens()`.
#'
#' Exact matches (e.g., `"st"`, `"rd."`, `"via"`) are always replaced.
#' Suffix matches (e.g., German `"strasse"` endings or Dutch `"straat"`)
#' are applied **only when `lang` is explicitly specified**, which prevents
#' unsafe substitutions such as rewriting the ending of `"LINCOLN LANE"`.
#'
#' @param x A character vector containing street names or address fragments.
#' @param lang Optional language code (e.g., `"de"`, `"en"`, `"fr"`).
#'   When provided, the dictionary is filtered to that language and safe
#'   language-specific suffix matching is enabled. It also restricts
#'   `drop_stopwords` to that language's particle list.
#' @param drop_house_numbers Logical (default `FALSE`). When `TRUE`, drops any
#'   token beginning with a digit (house numbers like `"12"`, `"12A"`,
#'   `"123B"`), keeping only the street name. Applied after street-type
#'   replacement.
#' @param drop_stopwords Logical (default `FALSE`). When `TRUE`, removes
#'   locative particles and articles (e.g. German `AN DER`, French `DE LA`)
#'   listed in `stopwords`, collapsing `"An der Alster"` to `"ALSTER"`. When
#'   `lang` is given, only that language's particles are removed; otherwise the
#'   whole `stopwords` set is used.
#' @param dict A dictionary of street-type definitions, typically
#'   [joinery::street_types], containing the columns:
#'   * `canonical`: canonical uppercase form
#'   * `variant`: lowercased normalized variant form
#'   * `type`: `"exact"` or `"suffix"`
#'   * `lang`: ISO language code
#' @param stopwords A street-stopword table, typically
#'   [joinery::street_stopwords], with columns `stopword` (uppercase ASCII) and
#'   `lang`. Only consulted when `drop_stopwords = TRUE`.
#'
#' @return A character vector of normalized street names. `NA` inputs are
#'   preserved as `NA`. Rows reduced to nothing (e.g. a bare house number with
#'   `drop_house_numbers = TRUE`) become `""`.
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
#' normalize_street("Hauptstr. 123A", lang = "de", drop_house_numbers = TRUE)
#' # "HAUPTSTRASSE"
#'
#' normalize_street("An der Alster 5", lang = "de",
#'                  drop_house_numbers = TRUE, drop_stopwords = TRUE)
#' # "ALSTER"
#'
#' @family text normalizers
#' @export
normalize_street <- function(x, lang = NULL,
                             drop_house_numbers = FALSE,
                             drop_stopwords = FALSE,
                             dict = joinery::street_types,
                             stopwords = joinery::street_stopwords) {

  check_character(x)
  check_bool(drop_house_numbers)
  check_bool(drop_stopwords)
  missing_cols <- setdiff(c("canonical", "variant", "type", "lang"), names(dict))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg dict} is missing required column{?s} {.field {missing_cols}}")
  }
  if (drop_stopwords) {
    missing_sw <- setdiff(c("stopword", "lang"), names(stopwords))
    if (length(missing_sw)) {
      cli::cli_abort("{.arg stopwords} is missing required column{?s} {.field {missing_sw}}")
    }
  }

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

  # Vectorized token replacement.
  #
  # Rather than calling a per-token closure inside a nested map (O(tokens)
  # R-level dispatch), flatten every row's tokens into one vector, resolve all
  # exact matches in a single `match()`, resolve suffix matches in one pass per
  # distinct suffix variant (longest-first, so the longest suffix wins exactly
  # as the scalar path did), then re-collapse per row. Semantics are identical
  # to the previous `replace_tok()`; only the loop nesting is gone.
  toks_list <- stri_split_regex(x_norm, " +")
  n_tok     <- lengths(toks_list)
  flat      <- unlist(toks_list, use.names = FALSE)   # uppercased token text
  flat_l    <- tolower(flat)
  result    <- flat                                   # default: token unchanged

  # Exact match (takes priority over suffix)
  ex_idx <- match(flat_l, names(exact_lookup))
  has_ex <- !is.na(ex_idx)
  result[has_ex] <- unname(exact_lookup[ex_idx[has_ex]])

  # Suffix match — only when lang is specified, only on non-exact tokens.
  if (!is.null(lang) && length(suffix_variants)) {
    remaining <- which(!has_ex & !is.na(flat_l))
    for (suf in suffix_variants) {
      if (!length(remaining)) break
      ends <- stri_endswith_fixed(flat_l[remaining], suf)
      hit  <- remaining[ends]
      if (length(hit)) {
        base <- stri_sub(flat[hit], 1, nchar(flat[hit]) - nchar(suf))
        result[hit] <- paste0(base, suffix_lookup[[suf]])
        remaining <- remaining[!ends]
      }
    }
  }

  # Optional token drops (house numbers / locative particles), applied to the
  # post-replacement flat vector before re-collapsing. NA tokens (from NA input
  # rows) never match either predicate, so they survive to the is_na overwrite.
  grp_int <- rep.int(seq_along(toks_list), n_tok)
  if (drop_house_numbers || drop_stopwords) {
    keep <- !is.na(flat)
    if (drop_house_numbers) {
      keep <- keep & !grepl("^[0-9]", flat)
    }
    if (drop_stopwords) {
      sw <- if (!is.null(lang)) stopwords$stopword[stopwords$lang == lang] else stopwords$stopword
      keep <- keep & !(result %in% sw)
    }
    keep[is.na(flat)] <- TRUE                 # carry NA tokens through
    result  <- result[keep]
    grp_int <- grp_int[keep]
  }

  # Re-collapse tokens back to one string per input row. A factor with explicit
  # 1:N levels keeps row order and yields "" for any group emptied by a drop.
  grp <- factor(grp_int, levels = seq_along(toks_list))
  out <- vapply(split(result, grp), paste, character(1), collapse = " ")
  out <- unname(out)

  out[is_na] <- NA_character_
  out
}

#' Normalize dates to ISO 8601 format (YYYY-MM-DD)
#'
#' The same day is written `"31.12.2023"`, `"12/31/2023"`, or `"2023-12-31"`
#' depending on who typed it. `normalize_date()` parses these mixed formats and
#' rewrites them to one ISO 8601 string (`YYYY-MM-DD`), so a date column matches
#' on the day it names rather than on how it was formatted. It recognizes
#' European (DD.MM.YYYY), American (MM/DD/YYYY), and ISO-style inputs.
#'
#' Returns text. For matching on individual date parts (year only, year and
#' month) use [date_tokens()]; to deliberately blur near-dates together use
#' [approximate_date()].
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
#' @family date preparers
#' @export
normalize_date <- function(x, format = NULL, orders = c("ymd", "dmy", "mdy")) {

  if (!is.character(x) && !inherits(x, "Date")) {
    cli::cli_abort("{.arg x} must be character or {.cls Date}")
  }
  if (!is.null(format)) check_character(format)
  check_character(orders)

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


#' Strip vowels from text (consonant skeleton)
#'
#' Reduces text to its consonant skeleton by removing vowels (A, E, I, O, U,
#' including accented variants). Two spellings that differ only in their vowels,
#' such as `"MEYER"` and `"MAYER"` or `"MUELLER"` and `"MULLER"`, collapse to the
#' same skeleton, so they match despite the difference. It is a lighter-weight
#' alternative to the phonetic encoders ([as_soundex()], [as_metaphone()]) when
#' you only want to ignore vowel variation.
#'
#' Returns text, so it goes ahead of a token generator in a pipeline.
#'
#' @param text A character vector.
#'
#' @return A character vector with vowels removed, upper-cased and ASCII-folded.
#'
#' @examples
#' strip_vowels("Mueller")   # "MLLR"
#' strip_vowels("Cafe Noir") # "CF NR"
#' strip_vowels(c("Anna", "Peter"))
#'
#' @family text normalizers
#' @seealso [as_soundex()] and [as_metaphone()] for full phonetic encoding.
#' @export
strip_vowels <- function(text) {
  check_character(text)

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


#' Encode text phonetically with Metaphone
#'
#' Names that sound alike are often spelled differently: `"Smith"` and
#' `"Smyth"`, `"Meyer"` and `"Maier"`. Metaphone encodes text by how it sounds,
#' so those variants share one key and match even though the letters differ.
#' Best on single-word fields such as surnames or company names; it is tuned for
#' English pronunciation (for German, see [as_cologne()]).
#'
#' Returns text, so it slots ahead of a token generator, or use it directly on a
#' one-word column. Phonetic keys are deliberately coarse, so they trade
#' precision for recall: pair them with a sharper field rather than matching on a
#' phonetic key alone.
#'
#' @param text A character string or vector to encode.
#'
#' @return A character vector of Metaphone keys, one per input element.
#'
#' @examples
#' as_metaphone("Smith")
#' as_metaphone(c("Meyer", "Maier"))  # same key
#'
#' @family phonetic encoders
#' @export
#' @import phonics
as_metaphone <- function(text) {
  # Normalize accents: Cafe (accented) -> Cafe
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")

  # Handle the German sharp-s (eszett, U+00DF) separately: ASCII//TRANSLIT turns
  # it into "ss" on some locales, but not reliably. Escaped to keep R source ASCII.
  x_norm <- gsub(intToUtf8(0x00DF), "ss", x_norm, ignore.case = TRUE)

  phonics::metaphone(x_norm,clean = FALSE)
}


#' Encode text phonetically with Soundex
#'
#' Soundex is the classic phonetic code: it keeps the first letter and reduces
#' the rest to a short digit string (for example `"Robert"` and `"Rupert"` both
#' become `"R163"`), so spellings that sound alike share one key. It is coarser
#' and older than Metaphone but widely understood and a good default for English
#' surnames.
#'
#' Returns text, so it slots ahead of a token generator, or use it directly on a
#' one-word column. As with any phonetic key it favours recall over precision;
#' pair it with a sharper field rather than matching on the key alone.
#'
#' @param text A character string or vector to encode.
#'
#' @return A character vector of Soundex keys (letter followed by digits), one
#'   per input element.
#'
#' @examples
#' as_soundex("Robert")
#' as_soundex(c("Robert", "Rupert"))  # same key
#'
#' @family phonetic encoders
#' @export
#' @import phonics
as_soundex <- function(text){
  check_character(text)

  # Normalize accents: Cafe (accented) -> Cafe
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")

  # Handle the German sharp-s (eszett, U+00DF) separately: ASCII//TRANSLIT turns
  # it into "ss" on some locales, but not reliably. Escaped to keep R source ASCII.
  x_norm <- gsub(intToUtf8(0x00DF), "ss", x_norm, ignore.case = TRUE)

  phonics::soundex(x_norm,clean = FALSE)
}


#' Encode text phonetically with the Cologne procedure
#'
#' The Cologne phonetic procedure (Koelner Phonetik) is the German-language
#' counterpart to Soundex. It maps text to a digit string by German
#' pronunciation rules, so variants like `"Meier"`, `"Maier"`, and `"Mayer"`
#' share one key. Reach for this over [as_soundex()] or [as_metaphone()] when the
#' data is German.
#'
#' Returns text, so it slots ahead of a token generator, or use it directly on a
#' one-word column. Like any phonetic key it favours recall over precision; pair
#' it with a sharper field rather than matching on the key alone.
#'
#' @param text A character string or vector to encode.
#'
#' @return A character vector of Cologne phonetic keys (digit strings), one per
#'   input element.
#'
#' @examples
#' as_cologne(c("Meier", "Maier", "Mayer"))  # same key
#'
#' @family phonetic encoders
#' @export
#' @import phonics
as_cologne <- function(text){
  check_character(text)

  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  x_norm <- gsub(intToUtf8(0x00DF), "ss", x_norm, ignore.case = TRUE)

  phonics::cologne(x_norm, clean = FALSE)
}


#' Map tokens to canonical groups with a lookup table
#'
#' When you already know which tokens mean the same thing (a curated synonym
#' list, brand-name variants, a code-to-label table), `use_dictionary()` rewrites
#' each token to its group label so the variants collapse to one token and match.
#' Use it when the mapping is known in advance; when you instead want joinery to
#' discover near-duplicates from the data, use [fuzzy_tokens()].
#'
#' Tokens absent from the dictionary return no group, so chain this after a token
#' generator and keep a sharper field alongside it.
#'
#' @param text A character vector of tokens to look up.
#' @param dict A [data.table::data.table] with a `tokens` column and a
#'   `token_group` column. Rows whose `tokens` value matches an input token
#'   supply that token's group label.
#'
#' @return A list of character vectors, one per input element, holding the
#'   matched group labels (empty when the token is not in `dict`).
#'
#' @examples
#' dict <- data.table::data.table(
#'   tokens = c("example", "sample"),
#'   token_group = c("example/sample", "example/sample")
#' )
#' use_dictionary("example", dict)
#' use_dictionary("nonexistent", dict)
#'
#' @family token transformers
#' @seealso [fuzzy_tokens()] to discover groups from the data instead.
#' @export
use_dictionary <- function(text, dict) {
  if (!data.table::is.data.table(dict)) {
    cli::cli_abort("{.arg dict} must be a {.cls data.table}")
  }

  lookup_row <- function(r){
    dict[tokens %in% r]$token_group
  }

  map(text,lookup_row)
}
