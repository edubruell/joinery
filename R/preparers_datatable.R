

#' Normalize text string
#'
#' This function converts a text string to upper case, transliterates it based on the specified
#' transliteration scheme, retains only alphanumeric characters and spaces, and removes extra spaces.
#'
#' @param .text A character string or vector to be normalized.
#' @param .transliteration A character string specifying the transliteration scheme to be used,
#'        defaulting to "De-ASCII".
#'
#' @return Returns a normalized, upper-case version of the input text, with non-alphanumeric characters
#'         and extra spaces removed.
#'
#' @examples
#' normalize_text("Café Coñac")
#' normalize_text("Straße", .transliteration = "Latin-ASCII")
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
#' Suffix matches (e.g., German `"straße"` endings or Dutch `"straat"`)
#' are applied **only when `lang` is explicitly specified**, preventing unsafe
#' substitutions such as `"LINCOLANE"` → `"LINCOLANE"`.
#'
#' @param x A character vector containing street names or address fragments.
#' @param lang Optional language code (e.g., `"de"`, `"en"`, `"fr"`).  
#'   When provided, the dictionary is filtered to that language and safe
#'   language-specific suffix matching is enabled.
#' @param dict A dictionary of street-type definitions, typically
#'   [joinery::street_types], containing the columns:
#'   * `canonical` — canonical uppercase form  
#'   * `variant`   — lowercased normalized variant form  
#'   * `type`      — `"exact"` or `"suffix"`  
#'   * `lang`      — ISO language code
#'
#' @return A character vector of normalized street names. `NA` inputs are
#'   preserved as `NA`.
#'
#' @details
#' Normalization steps include:
#' * Unicode → Latin transliteration and ASCII folding (`stri_trans_general`)
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
#' normalize_street("Müllerstraße", lang = "de")
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

#' Strip vowels from text
#'
#' Removes vowels (A, E, I, O, U) including accented and umlaut variants.
#' Useful for fuzzy matching (e.g. "MÜLLER" -> "MLLR", "JOSÉ" -> "JS").
#'
#' @param text Character vector.
#'
#' @return Character vector with vowels removed.
#'
#' @examples
#' strip_vowels("Müller")   # "MLLR"
#' strip_vowels("Café Noir") # "CF NR"
#' strip_vowels(c("Anna", "Peter"))
#'
#' @export
strip_vowels <- function(text) {
  c("text must be character" = is.character(text)) |>
    validate_inputs()
  
  if (length(text) == 0L) return(text)
  
  # Normalize accents → Latin → ASCII
  x <- text |>
    stringi::stri_trans_general("Any-Latin") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper()
  
  # Remove vowels A/E/I/O/U
  # Includes letter Y only if you want it – typical Germanic/Romance matching leaves Y in.
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
#' as_metaphone("Café")
#' as_metaphone("Straße")
#' @export
#' @import phonics
as_metaphone <- function(text) {
  # Normalize accents: Café → Cafe
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
#' as_soundex("Café")
#' as_soundex("Straße")
#' @export
#' @import phonics
as_soundex <- function(text){
  # Validate inputs to the function
  c("Input text must be a string" = is.character(text)
  ) |>
    validate_inputs()
  
  # Normalize accents: Café → Cafe
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
#' as_cologne("Café")
#' as_cologne("Straße")
#' @export
#' @import phonics
as_cologne <- function(text){
  c("Input text must be a string" = is.character(text)) |>
    validate_inputs()
  
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)
  
  phonics::cologne(x_norm, clean = FALSE)
}


#' Return a list of word tokens for the .text separated by spaces.
#'
#' This function splits the input text into words based on spaces. It returns a vector of the words
#' found in the text. This function is useful for natural language processing tasks where word-level
#' manipulation of text is required.
#'
#' @param text A character string from which words will be extracted.
#' @param min_nchar An integer specifying the minimum length of words to keep. Defaults to 0.
#'
#' @return Returns a vector of words extracted from the input text.
#'
#' @examples
#' word_tokens("This is an example.")
#' word_tokens("Another, test; string.")
#' @export
word_tokens <- function(text,min_nchar=0){
  # Validate inputs to the function
  c("Input text must be a string" = is.character(text),
    "Input min_nchar must be a integer valued numeric" = min_nchar == as.integer(min_nchar)
  ) |>
    validate_inputs()
  
  # Split the text into words based on spaces
  words <- strsplit(text, "\\s+")
  # Remove empty elements if any (this can happen with multiple spaces)
  words <- map(words, function(x) x[nzchar(x)])
  
  # Filter out words shorter than .min_length
  if (min_nchar > 0) {
    words <- map(words,function(x){
      filter <- nchar(x)>=min_nchar
      x[filter]
    })
  }
  return(words)
}


#' Generate n-grams from text
#'
#' This function generates n-grams from a given text string. An n-gram is a contiguous sequence of n items
#' from a given sample of text or speech. This function will return a list of all possible n-grams of length n.
#'
#' @param text A character string or vector from which to generate n-grams.
#' @param n An integer specifying the length of each n-gram.
#'
#' @return Returns a list of n-grams generated from the input text. If the text length is less than n, returns
#'         an empty character vector.
#'
#' @examples
#' generate_ngrams("hello", 2)
#' generate_ngrams("an example", 3)
#' @export
#' @import data.table
#' @import stringi
generate_ngrams <- function(text, n) {
  # Validate inputs
  c(
    "Input text must be a string" = is.character(text),
    "Input n must be an integer valued numeric" = n == as.integer(n)
  ) |>
    validate_inputs()
  
  int_df <- data.table::data.table(text = text)
  int_df[, len_text := stri_length(text)]
  
  # Generate n-grams for one string using purrr-shim syntax
  generate_ngrams_single <- function(s) {
    len_s <- stri_length(s)
    if (len_s < n) return(character(0))
    
    map_chr(
      seq_len(len_s - n + 1),
      ~ stri_sub(s, .x, .x + n - 1)
    )
  }
  
  # Apply with purrr-shim map()
  int_df[, ngrams := map(text, generate_ngrams_single)]
  
  int_df$ngrams
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
#' dict <- data.table(tokens = c("example", "sample"), token_group = c("example/sample", "example/sample"))
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


#' Tokenize numeric fields, expanding ranges into individual numbers
#'
#' @description
#' Turns numeric/house-number–like text into a list of tokens.
#' Expands ranges such as "12-14" or "7–9" into c("12","13","14").
#' Uses original spacing/separators to detect ranges, while normalization
#' cleans text for tokenization.
#'
#' @param text Character vector of numeric or address fields.
#' @param keep_letters Logical. If TRUE, retains letter suffixes like "12A".
#'   Only applies when `destructive = FALSE`.
#' @param destructive Logical. If TRUE, removes all non-digit characters
#'   except whitespace. If FALSE (default), preserves letters alongside digits.
#'
#' @return A list of character vectors, one per input element. Each vector
#'   contains numeric tokens, with ranges expanded into sequences.
#'
#' @examples
#' numeric_tokens("12-14")
#' # list(c("12", "13", "14"))
#'
#' numeric_tokens("7A 9B", keep_letters = TRUE)
#' # list(c("7A", "9B"))
#'
#' numeric_tokens("House 5", destructive = TRUE)
#' # list("5")
#'
#' @export
numeric_tokens <- function(text,
                           keep_letters = TRUE,
                           destructive = FALSE) {

  c("text must be character" = is.character(text)) |>
    validate_inputs()

  # Original text (needed for range detection)
  raw <- text

  # Base normalization
  x <- stri_trans_general(text, "Any-Latin") |>
       stri_trans_general("Latin-ASCII") |>
       stri_trans_toupper()

  if (destructive) {
    # Keep only digits and whitespace
    x <- stri_replace_all_regex(x, "[^0-9]", " ")
  } else {
    # Keep digits, letters, and whitespace
    x <- stri_replace_all_regex(x, "[^0-9A-Z]", " ")
  }

  x <- stri_trim_both(x)
  x <- stri_replace_all_regex(x, "\\s+", " ")

  tokenize_one <- function(s_raw, s_clean) {

    if (is.na(s_clean) || !nzchar(s_clean)) return(character(0))

    parts <- unlist(strsplit(s_clean, "\\s+"))
    parts <- parts[nzchar(parts)]

    out <- character()

    # --- Range detection based on cleaned form ---
    if (grepl("^[0-9]+ [0-9]+$", s_clean)) {
      bounds <- unlist(strsplit(s_clean, " "))
      lo <- as.integer(bounds[1])
      hi <- as.integer(bounds[2])

      if (!is.na(lo) && !is.na(hi) && lo <= hi) {
        return(as.character(seq(lo, hi)))
      }
    }

    # --- Token-by-token interpretation ---
    for (p in parts) {

      # number-letter tokens (only allowed in non-destructive mode)
      if (!destructive && keep_letters && grepl("^[0-9]+[A-Z]$", p)) {
        out <- c(out, p)
        next
      }

      # pure number
      if (grepl("^[0-9]+$", p)) {
        out <- c(out, p)
        next
      }
    }

    unique(out)
  }

  map2(raw, x, tokenize_one)
}


#' Filter out stopwords from token lists
#'
#' Removes tokens that appear in a stopword list. Works on list-of-character
#' token vectors produced by earlier steps such as `word_tokens()`.
#'
#' @param tokens A list of character vectors.
#' @param stopwords A character vector of stopwords (case-insensitive).
#'
#' @return A list of character vectors with stopwords removed.
#' @export
filter_stopwords <- function(tokens, stopwords) {
  c("tokens must be a list" = is.list(tokens),
    "stopwords must be character" = is.character(stopwords)) |>
    validate_inputs()
  
  sw <- toupper(stopwords)
  
  map(tokens, function(x) {
    x_up <- toupper(x)
    x[!(x_up %in% sw)]
  })
}

#' Convert tokens to shape signatures (letter/digit patterns)
#'
#' "MULLER" -> "AAAAAA"
#' "A12B"   -> "ANNA"
#'
#' @param tokens A list of character vectors.
#'
#' @return A list of shape tokens.
#' @export
token_shapes <- function(tokens) {
  c("tokens must be a list" = is.list(tokens)) |> validate_inputs()
  
  map(tokens, function(x) {
    map_chr(x, function(tok) {
      chars <- strsplit(tok, "")[[1]]
      out <- ifelse(grepl("[A-Z]", chars), "A",
                    ifelse(grepl("[0-9]", chars), "N", "X"))
      paste(out, collapse = "")
    })
  })
}

#' Extract initials from tokens
#'
#' Converts tokens to their first-letter initial ("ANNA" -> "A").
#'
#' @param tokens A list of character vectors.
#'
#' @return A list of character vectors of initials.
#' @export
extract_initials <- function(tokens) {
  c("tokens must be a list" = is.list(tokens)) |> validate_inputs()
  
  map(tokens, function(x) {
    map_chr(x, function(tok) substr(tok, 1, 1))
  })
}

#' Fuzzy tokens using igraph components (fast, sparse)
#'
#' @param text Character vector
#' @param min_nchar Minimum token size
#' @param max_dist Maximum string distance to consider an edge
#' @param method stringdist method ("osa", "lv", "jw", ...)
#'
#' @return List of fuzzy tokens (list-column)
#' @export
fuzzy_tokens <- function(x,
                         max_dist = 2,
                         method = "osa",
                         min_nchar = 1) {
  
  # ---------------------------------------------------------------------------
  # Early exit for NA rows
  # ---------------------------------------------------------------------------
  is_na <- is.na(x)
  x_clean <- x
  x_clean[is_na] <- ""
  
  # ---------------------------------------------------------------------------
  # Tokenize
  # ---------------------------------------------------------------------------
  toks <- map(x_clean, function(s) {
    if (!nzchar(s)) return(character(0))
    out <- unlist(strsplit(s, "\\s+"))
    out <- toupper(out)
    out[nchar(out) >= min_nchar]
  })
  
  # If all empty, return early
  if (all(lengths(toks) == 0) && all(is_na)) {
    return(map(x, function(val) if (is.na(val)) NA_character_ else character(0)))
  }
  
  # ---------------------------------------------------------------------------
  # Collect ALL unique tokens globally
  # ---------------------------------------------------------------------------
  all_tokens <- unique(unlist(toks, use.names = FALSE))
  
  # trivial case
  if (length(all_tokens) <= 1L) {
    return(map(toks, function(.x) {
      if (length(.x) == 0) character(0) else all_tokens
    }))
  }
  
  # ---------------------------------------------------------------------------
  # Compute full stringdist matrix globally
  # ---------------------------------------------------------------------------
  D <- stringdist::stringdistmatrix(all_tokens, all_tokens, method = method)
  
  # For JW: max_dist is similarity threshold → convert accordingly
  if (method == "jw" && max_dist < 1) {
    # jw distance = 1 - similarity
    dist_threshold <- max_dist
  } else {
    dist_threshold <- max_dist
  }
  
  # ---------------------------------------------------------------------------
  # Build fuzzy adjacency (global graph)
  # ---------------------------------------------------------------------------
  # Remove diagonal
  diag(D) <- Inf
  
  # Strict distance cutoff
  edges <- which(D <= dist_threshold, arr.ind = TRUE)
  
  # Keep only upper triangle (avoid double edges)
  edges <- edges[edges[,1] < edges[,2], , drop = FALSE]
  
  # Build graph
  g <- igraph::make_empty_graph(n = length(all_tokens), directed = FALSE)
  if (nrow(edges) > 0) {
    g <- igraph::add_edges(g, t(edges)) 
  }
  
  comp <- igraph::components(g)$membership
  groups <- split(all_tokens, comp)
  groups <- unname(groups)
  
  # ---------------------------------------------------------------------------
  # Canonical selection function:
  # longest → min mean distance → lexicographically smallest
  # ---------------------------------------------------------------------------
  choose_canon <- function(g) {
    if (length(g) == 1L) return(g)
    
    n <- nchar(g)
    Dg <- stringdist::stringdistmatrix(g, g, method = method)
    center <- rowMeans(Dg)
    
    g[order(-n, center, g)][1]
  }
  
  canon <- map_chr(groups, choose_canon)
  
  # map canonical back to tokens
  canon_map <- unlist(
    map2(groups, canon, function(g, c) setNames(rep(c, length(g)), g)),
    use.names = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Rewrite each row's tokens
  # ---------------------------------------------------------------------------
  out <- map(toks, function(v) {
    if (!length(v)) return(character(0))
    unname(canon_map[v])
  })
  
  # Restore NA rows
  out[is_na] <- list(NA_character_)
  
  out
}


