# ============================================================
# Token-level preparers
# ============================================================
#
# Functions that either produce tokens from text, or transform existing
# token lists. Token lists are list-columns of character vectors keyed by
# row.
# ============================================================


#' Extract date components as tokens
#'
#' `date_tokens()` parses dates and extracts specified components (year, month, day)
#' as separate tokens. This is useful for flexible date matching where you want to
#' match on specific date parts rather than full dates.
#'
#' @param x A character or Date vector containing dates to tokenize.
#' @param components Character vector specifying which date components to extract.
#'   Can include `"year"`, `"month"`, and/or `"day"`. Defaults to all three.
#' @param format Optional format string for parsing (passed to `as.Date()`).
#'   If `NULL` (default), attempts automatic parsing via lubridate.
#' @param orders Optional character vector of lubridate order specifications
#'   (e.g., `c("dmy", "mdy", "ymd")`). Used when `format = NULL`.
#'   Defaults to `c("ymd", "dmy", "mdy")`.
#'
#' @return A list of character vectors, one per input element. Each vector
#'   contains the requested date components as strings. Unparseable dates
#'   return an empty character vector with a warning.
#'
#' @details
#' Components are returned as zero-padded strings:
#' * `"year"` -- 4-digit year (e.g., `"2023"`)
#' * `"month"` -- 2-digit month (e.g., `"01"`, `"12"`)
#' * `"day"` -- 2-digit day (e.g., `"05"`, `"31"`)
#'
#' The order of tokens in the output follows the order of `components`.
#'
#' @examples
#' date_tokens("2023-12-31")
#' # list(c("2023", "12", "31"))
#'
#' date_tokens("31.12.2023", components = c("year", "month"))
#' # list(c("2023", "12"))
#'
#' date_tokens("12/31/2023", components = "year")
#' # list("2023")
#'
#' date_tokens(c("2023-01-15", "15.06.2023"))
#' # list(c("2023", "01", "15"), c("2023", "06", "15"))
#'
#' @export
date_tokens <- function(x,
                        components = c("year", "month", "day"),
                        format = NULL,
                        orders = c("ymd", "dmy", "mdy")) {

  if (!is.character(x) && !inherits(x, "Date")) {
    cli::cli_abort("{.arg x} must be character or {.cls Date}")
  }
  check_character(components)
  bad <- setdiff(components, c("year", "month", "day"))
  if (length(bad)) {
    cli::cli_abort("{.arg components} contains invalid value{?s} {.val {bad}}")
  }
  if (!is.null(format)) check_character(format)
  check_character(orders)

  is_na <- is.na(x)

  if (inherits(x, "Date")) {
    parsed <- x
  } else if (!is.null(format)) {
    parsed <- as.Date(x, format = format)
    if (any(is.na(parsed) & !is_na)) {
      warning("Some dates could not be parsed with the specified format")
    }
  } else {
    parsed <- lubridate::parse_date_time(x, orders = orders, quiet = TRUE)
    parsed <- as.Date(parsed)
    if (any(is.na(parsed) & !is_na)) {
      warning("Some dates could not be parsed with orders: ", paste(orders, collapse = ", "))
    }
  }

  extract_components <- function(date) {
    if (is.na(date)) return(character(0))

    tokens <- character()

    for (comp in components) {
      token <- switch(comp,
        year = format(date, "%Y"),
        month = format(date, "%m"),
        day = format(date, "%d")
      )
      tokens <- c(tokens, token)
    }

    tokens
  }

  map(parsed, extract_components)
}

#' Approximate dates by rounding to coarser time units
#'
#' `approximate_date()` rounds dates to the start of broader time periods
#' (month, quarter, half-year, year, or decade). This is useful for fuzzy
#' temporal matching when exact dates may differ slightly but represent the
#' same general time period.
#'
#' @param x A character or Date vector containing dates to approximate.
#' @param unit Character string specifying the rounding unit. One of:
#'   * `"month"` -- round to first day of month (default)
#'   * `"quarter"` -- round to first day of quarter (Jan 1, Apr 1, Jul 1, Oct 1)
#'   * `"half"` -- round to first day of half-year (Jan 1 or Jul 1)
#'   * `"year"` -- round to January 1
#'   * `"decade"` -- round to first year of decade (e.g., 2020-01-01)
#' @param format Optional format string for parsing (passed to `as.Date()`).
#'   If `NULL` (default), attempts automatic parsing via lubridate.
#' @param orders Optional character vector of lubridate order specifications.
#'   Used when `format = NULL`. Defaults to `c("ymd", "dmy", "mdy")`.
#'
#' @return A character vector of dates in ISO 8601 format (YYYY-MM-DD),
#'   rounded to the start of the specified time unit. Unparseable dates
#'   return `NA_character_` with a warning.
#'
#' @details
#' Rounding always goes to the **start** of the period:
#' * `"month"`: 2023-03-15 -> 2023-03-01
#' * `"quarter"`: 2023-03-15 -> 2023-01-01 (Q1), 2023-05-20 -> 2023-04-01 (Q2)
#' * `"half"`: 2023-03-15 -> 2023-01-01 (H1), 2023-08-20 -> 2023-07-01 (H2)
#' * `"year"`: 2023-03-15 -> 2023-01-01
#' * `"decade"`: 2023-03-15 -> 2020-01-01
#'
#' @examples
#' approximate_date("2023-03-15", unit = "month")
#' # "2023-03-01"
#'
#' approximate_date("2023-03-15", unit = "quarter")
#' # "2023-01-01"
#'
#' approximate_date("2023-08-20", unit = "half")
#' # "2023-07-01"
#'
#' approximate_date("2023-03-15", unit = "year")
#' # "2023-01-01"
#'
#' approximate_date("2023-03-15", unit = "decade")
#' # "2020-01-01"
#'
#' approximate_date(c("2023-01-15", "2023-04-20", "2023-09-10"), unit = "quarter")
#' # c("2023-01-01", "2023-04-01", "2023-07-01")
#'
#' @export
approximate_date <- function(x,
                             unit = c("month", "quarter", "half", "year", "decade"),
                             format = NULL,
                             orders = c("ymd", "dmy", "mdy")) {

  unit <- match.arg(unit)

  if (!is.character(x) && !inherits(x, "Date")) {
    cli::cli_abort("{.arg x} must be character or {.cls Date}")
  }
  if (!is.null(format)) check_character(format)
  check_character(orders)

  is_na <- is.na(x)

  if (inherits(x, "Date")) {
    parsed <- x
  } else if (!is.null(format)) {
    parsed <- as.Date(x, format = format)
    if (any(is.na(parsed) & !is_na)) {
      warning("Some dates could not be parsed with the specified format")
    }
  } else {
    parsed <- lubridate::parse_date_time(x, orders = orders, quiet = TRUE)
    parsed <- as.Date(parsed)
    if (any(is.na(parsed) & !is_na)) {
      warning("Some dates could not be parsed with orders: ", paste(orders, collapse = ", "))
    }
  }

  round_date <- function(date) {
    if (is.na(date)) return(NA_character_)

    year <- as.integer(format(date, "%Y"))
    month <- as.integer(format(date, "%m"))

    result <- switch(unit,
      month = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),

      quarter = {
        q_month <- ((month - 1) %/% 3) * 3 + 1
        as.Date(paste0(year, "-", sprintf("%02d", q_month), "-01"))
      },

      half = {
        h_month <- if (month <= 6) 1 else 7
        as.Date(paste0(year, "-", sprintf("%02d", h_month), "-01"))
      },

      year = as.Date(paste0(year, "-01-01")),

      decade = {
        decade_year <- (year %/% 10) * 10
        as.Date(paste0(decade_year, "-01-01"))
      }
    )

    format(result, "%Y-%m-%d")
  }

  map_chr(parsed, round_date)
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
  check_character(text)
  check_number_whole(min_nchar, min = 0)

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
  check_character(text)
  check_number_whole(n, min = 1)

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


#' Tokenize numeric fields, expanding ranges into individual numbers
#'
#' @description
#' Turns numeric/house-number-like text into a list of tokens.
#' Expands ranges such as "12-14" or "7-9" into c("12","13","14").
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

  check_character(text)

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
  if (!is.list(tokens)) {
    cli::cli_abort("{.arg tokens} must be a list")
  }
  check_character(stopwords)

  sw <- toupper(stopwords)

  map(tokens, function(x) {
    x_up <- toupper(x)
    x[!(x_up %in% sw)]
  })
}

#' Drop numeric (house-number) tokens from token lists
#'
#' @description
#' Symmetric inverse of [numeric_tokens()]: removes pure-digit tokens
#' (typically house numbers) from a token column. Operates on the
#' list-of-character token vectors produced by earlier steps such as
#' `word_tokens()`, mirroring [filter_stopwords()].
#'
#' Useful in address pipelines where the street name carries the matching
#' signal but the house number is noise (and fans out blocks): tokenize the
#' street, then `drop_numeric_tokens()` to keep only the name tokens.
#'
#' @param tokens A list of character vectors.
#' @param keep_letters Logical. If TRUE (default), number-letter tokens such
#'   as "12A" are retained; only pure-digit tokens like "12" are dropped. If
#'   FALSE, any token containing a digit is dropped.
#'
#' @return A list of character vectors with numeric tokens removed.
#'
#' @examples
#' drop_numeric_tokens(list(c("MAIN", "12", "ST")))
#' # list(c("MAIN", "ST"))
#'
#' drop_numeric_tokens(list(c("MAIN", "12A")), keep_letters = FALSE)
#' # list("MAIN")
#'
#' @export
drop_numeric_tokens <- function(tokens, keep_letters = TRUE) {
  if (!is.list(tokens)) {
    cli::cli_abort("{.arg tokens} must be a list")
  }

  # Pure-digit tokens are always dropped. With keep_letters = FALSE, any token
  # containing a digit (e.g. "12A") is dropped too. Vectorized grepl over the
  # flat token vector per element, mirroring filter_stopwords().
  pattern <- if (keep_letters) "^[0-9]+$" else "[0-9]"

  map(tokens, function(x) {
    x[!grepl(pattern, x)]
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
  if (!is.list(tokens)) {
    cli::cli_abort("{.arg tokens} must be a list")
  }

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
  if (!is.list(tokens)) {
    cli::cli_abort("{.arg tokens} must be a list")
  }

  map(tokens, function(x) {
    map_chr(x, function(tok) substr(tok, 1, 1))
  })
}

#' Fuzzy tokens using igraph components (fast, sparse)
#'
#' @param x Character vector
#' @param min_nchar Minimum token size
#' @param max_dist Maximum string distance to consider an edge
#' @param method stringdist method ("osa", "lv", "jw", ...)
#'
#' @return List of fuzzy tokens (list-column)
#' @importFrom stats setNames
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

  # For JW: max_dist is similarity threshold -> convert accordingly
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
  # longest -> min mean distance -> lexicographically smallest
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
