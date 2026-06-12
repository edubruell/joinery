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
#' @family date preparers
#' @seealso [normalize_date()] to match whole dates, [approximate_date()] to
#'   match on coarser periods.
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
#' @family date preparers
#' @seealso [normalize_date()] for exact dates, [date_tokens()] to split a date
#'   into part tokens.
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


#' Split text into word tokens
#'
#' The workhorse tokenizer. It splits each string on whitespace into a vector of
#' words, the tokens joinery matches on. It almost always follows
#' [normalize_text()], which strips punctuation and case first so the split is
#' clean: `name ~ normalize_text() + word_tokens()`.
#'
#' Set `min_nchar` to drop very short tokens (single initials, stray letters)
#' that match too easily and add noise.
#'
#' @param text A character vector to split into words.
#' @param min_nchar Minimum token length to keep. Tokens shorter than this are
#'   dropped. Defaults to `0` (keep everything).
#'
#' @return A list of character vectors, one per input element, each holding that
#'   element's word tokens.
#'
#' @examples
#' word_tokens("this is an example")
#' word_tokens("this is an example", min_nchar = 3)  # drops "is", "an"
#'
#' @family token generators
#' @seealso [normalize_text()], the usual preceding step;
#'   [filter_stopwords()] to drop common words by name.
#' @export
word_tokens <- function(text,min_nchar=0){
  check_character(text)
  check_number_whole(min_nchar, min = 0)

  # Split the text into words based on spaces
  words <- strsplit(text, "\\s+")
  # Remove empty elements if any (this can happen with multiple spaces)
  words <- map(words, function(x) x[nzchar(x)])

  # Filter out words shorter than min_nchar
  if (min_nchar > 0) {
    words <- map(words,function(x){
      filter <- nchar(x)>=min_nchar
      x[filter]
    })
  }
  return(words)
}


#' Generate character n-grams from text
#'
#' An n-gram is a sliding window of `n` consecutive characters. Matching on
#' character n-grams instead of whole words tolerates typos, truncations, and
#' joined-up spellings, because two strings that differ by a letter still share
#' most of their windows (`"meier"` and `"maier"` share `"ei"`, `"er"`, and so
#' on). Reach for it on short, noisy fields where word tokens are too brittle.
#'
#' It tokenizes text directly, so it replaces [word_tokens()] rather than
#' following it. The trade-off is fan-out: every string yields many overlapping
#' tokens, so n-grams cost more to match than words. Larger `n` is sharper and
#' cheaper, smaller `n` is fuzzier and denser.
#'
#' @param text A character vector to break into n-grams.
#' @param n The window length (number of characters per n-gram).
#'
#' @return A list of character vectors, one per input element. Strings shorter
#'   than `n` yield an empty vector.
#'
#' @examples
#' generate_ngrams("hello", 2)
#' generate_ngrams("an example", 3)
#'
#' @family token generators
#' @seealso [word_tokens()] for whole-word tokens.
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
#' @family token generators
#' @seealso [drop_numeric_tokens()], its inverse, to discard numbers from a
#'   token column instead.
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
#' Some tokens carry no matching signal but appear everywhere: legal forms like
#' `GMBH` or `LTD`, articles, generic words. Because they are common they create
#' many spurious matches and fan out blocks. `filter_stopwords()` removes named
#' tokens so matching rests on the distinctive ones. The comparison is
#' case-insensitive.
#'
#' It transforms a token column, so it runs after a token generator such as
#' [word_tokens()].
#'
#' @param tokens A list of character vectors, as produced by [word_tokens()].
#' @param stopwords A character vector of tokens to remove (case-insensitive).
#'
#' @return A list of character vectors with the stopwords removed.
#'
#' @examples
#' filter_stopwords(list(c("MUELLER", "GMBH")), stopwords = c("gmbh"))
#' # list("MUELLER")
#'
#' @family token transformers
#' @seealso [drop_numeric_tokens()] to remove house numbers the same way.
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
#' @family token transformers
#' @seealso [numeric_tokens()], its inverse; [filter_stopwords()] for the same
#'   idea with a named word list.
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

#' Convert tokens to shape signatures
#'
#' Reduces each token to its letter/digit pattern: every letter becomes `"A"`,
#' every digit `"N"`, anything else `"X"`. The signature ignores the actual
#' characters and keeps only the layout, which is useful for matching on the
#' format of a code or identifier (postal codes, licence plates, product codes)
#' rather than its exact value, or as a coarse blocking key.
#'
#' It transforms a token column, so it runs after a token generator such as
#' [word_tokens()].
#'
#' @param tokens A list of character vectors.
#'
#' @return A list of character vectors of shape signatures, one signature per
#'   input token.
#'
#' @examples
#' token_shapes(list(c("MUELLER", "A12B")))
#' # list(c("AAAAAAA", "ANNA"))
#'
#' @family token transformers
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
#' Keeps only the first character of each token (`"ANNA"` becomes `"A"`). Use it
#' to match on initials when full first names are recorded inconsistently, for
#' example when one source has `"Anna Berta Schmidt"` and another `"A. B.
#' Schmidt"`.
#'
#' It transforms a token column, so it runs after a token generator such as
#' [word_tokens()].
#'
#' @param tokens A list of character vectors.
#'
#' @return A list of character vectors of single-character initials.
#'
#' @examples
#' extract_initials(list(c("Anna", "Berta")))
#' # list(c("A", "B"))
#'
#' @family token transformers
#' @export
extract_initials <- function(tokens) {
  if (!is.list(tokens)) {
    cli::cli_abort("{.arg tokens} must be a list")
  }

  map(tokens, function(x) {
    map_chr(x, function(tok) substr(tok, 1, 1))
  })
}

#' Collapse near-duplicate tokens to a canonical form
#'
#' Typos and minor spelling differences split one real token into many
#' (`"Neumann"`, `"Neumann"` with a slip, `"Neuman"`). `fuzzy_tokens()` finds
#' tokens within a string distance of each other, groups them, and rewrites
#' every member of a group to one canonical spelling, so the variants match.
#' Unlike [use_dictionary()], which needs a known synonym list, this discovers
#' the groups from the data.
#'
#' Use it when a field has organic spelling noise and you do not have a
#' dictionary. The canonical form per group is the longest token, breaking ties
#' by the most central token, then alphabetically.
#'
#' When not to use it:
#' * **High-cardinality columns.** It compares every distinct token against
#'   every other in one dense distance matrix, so cost and memory grow with the
#'   square of the number of distinct tokens. On a large vocabulary (tens of
#'   thousands of distinct tokens and up) it is slow and memory-hungry.
#'   Normalize aggressively first, and prefer [use_dictionary()] when the groups
#'   are already known.
#' * **When over-merging is costly.** Grouping is by connected components, so
#'   matches chain transitively: if `A` is close to `B` and `B` to `C`, all
#'   three collapse even when `A` and `C` are far apart. A loose `max_dist` or
#'   short tokens can fuse genuinely distinct values. Keep `max_dist` tight,
#'   raise `min_nchar` to drop noise-prone short tokens, and check the groups on
#'   a sample before trusting them.
#'
#' @param x A character vector to tokenize and canonicalize.
#' @param max_dist Maximum string distance for two tokens to be treated as the
#'   same. For `method = "jw"` this is a Jaro-Winkler distance in `[0, 1]`
#'   (smaller is stricter); for edit-distance methods it is a count of edits.
#' @param method A [stringdist::stringdist()] method, e.g. `"osa"` (default),
#'   `"lv"`, or `"jw"`.
#' @param min_nchar Minimum token length to consider; shorter tokens are dropped
#'   before grouping.
#'
#' @return A list of character vectors, one per input element, with each token
#'   replaced by its group's canonical form.
#'
#' @examples
#' fuzzy_tokens(c("Neumann", "Neumaxn", "Neuman"), max_dist = 2)
#' # every row's token becomes "NEUMANN"
#'
#' @family token transformers
#' @seealso [use_dictionary()] when the groups are known in advance.
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
