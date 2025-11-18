
DT_tbl <- new_S3_class("data.table")

# Method: prepare_search_data for data.table, character ID, and Search_Strategy
#------------------------------------------------------------------------------
method(
   prepare_search_data,
    list(DT_tbl, class_character, Search_Strategy)
  ) <- function(data, id, strategy) {
  dt <- data.table::copy(data)
  
  if (!id %in% names(dt)) {
    stop(sprintf("ID column '%s' not found in data", id), call. = FALSE)
  }
  
  preparers <- strategy@preparers
  block_by  <- strategy@block_by
  
  # Helper: apply one Step (R backend)
  apply_step_r <- function(acc, step) {
    fn <- get(step@name, mode = "function")
    args <- c(list(acc), step@args)
    do.call(fn, args)
  }
  
  # One token table per prepared column -----------------------------------
  token_list <- map(preparers, function(prep) {
    col <- prep@column
    
    if (!col %in% names(dt)) {
      stop(sprintf("Column '%s' not found in data", col), call. = FALSE)
    }
    
    # Run pipeline on vector dt[[col]]
    tokens <- Reduce(
      f = apply_step_r,
      x = prep@steps,
      init = dt[[col]]
    )
    
    # Ensure list-of-character per row
    if (!is.list(tokens)) {
      tokens <- as.list(tokens)
    }
    
    lens <- lengths(tokens)
    
    # Build long token table WITHOUT any := or !!
    out <- data.table::data.table(
      column = col,
      token  = unlist(tokens, use.names = FALSE),
      row_id = rep(seq_len(nrow(dt)), times = lens)
    )
    
    # Add ID column with correct *name* (id is a character scalar)
    out[[id]] <- rep(dt[[id]], times = lens)
    
    # Reorder columns to have ID first
    data.table::setcolorder(out, c(id, "column", "token", "row_id"))
    
    out
  })
  
  tokens <- data.table::rbindlist(token_list, use.names = TRUE, fill = TRUE)
  
  # Attach blocking columns (if any) --------------------------------------
  if (!is.null(block_by)) {
    missing <- setdiff(block_by, names(dt))
    if (length(missing) > 0) {
      stop(
        "Blocking columns not found in data: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    
    block_dt <- dt[, c(id, block_by), with = FALSE]
    tokens   <- merge(tokens, block_dt, by = id, all.x = TRUE)
  }
  
  tokens[]
}


# Method: compute_rarity for data.table and Search_Strategy
#------------------------------------------------------------------------------
method(
  compute_rarity,
  list(DT_tbl, Search_Strategy)
) <- function(tokens, strategy) {
  
  dt <- data.table::copy(tokens)
  rarity_method <- strategy@rarity
  block_by      <- strategy@block_by
  
  # Grouping keys: block + column + token
  by_keys <- c(block_by, "column", "token")
  
  # Ensure block columns exist if block_by was specified
  if (!is.null(block_by)) {
    missing <- setdiff(block_by, names(dt))
    if (length(missing) > 0) {
      stop("Block columns missing: ", paste(missing, collapse = ", "))
    }
  }
  
  # Compute freq + df + N per block/column/token ---------------------------
  dt[, freq := .N, by = by_keys]
  
  # df = number of distinct rows in this block/column where token appears
  dt[, df := uniqueN(row_id), by = by_keys]
  
  # N = total rows in this block/column
  # (we attach once per group; downstream summed over matches)
  dt[, N := uniqueN(row_id), by = c(block_by, "column")]
  
  # Apply rarity formula ---------------------------------------------------
  dt[, rarity := {
    f  <- freq
    d  <- df
    n  <- N
    
    switch(
      rarity_method,
      "inverse_freq" = 1 / f,
      "tfidf" = {
        tf <- f / sum(f)
        idf <- log(1 + n / d)
        tf * idf
      },
      "smoothed_inverse_freq" = 1 / (f + 1),
      "bm25"         = log((n - d + 0.5) / (d + 0.5)),
      stop("Unknown rarity method: ", rarity_method)
    )
  }]
  
  dt[]
}


# Method: detect_duplicates 
#------------------------------------------------------------------------------
method(
  detect_duplicates,
  list(DT_tbl, class_character ,Search_Strategy, class_numeric)
) <- function(base_table, id, strategy, threshold, weights = NULL) {
  
  dt <- data.table::copy(base_table)
  
  # --- 1. Prepare token table ---------------------------------------------
  tokens <- prepare_search_data(
    data     = dt,
    id       = id,
    strategy = strategy
  )
  
  # --- 2. Compute rarity ---------------------------------------------------
  tokens <- compute_rarity(tokens, strategy)
  
  # --- 3. Determine weights -----------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights) > 0) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }
  
  # Guarantee weight coverage
  missing_w <- setdiff(unique(tokens$column), names(weights))
  if (length(missing_w) > 0) {
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
  }
  
  # Add weight column
  tokens[, weight := weights[column]]
  
  #Compute Identification Potential based on rarity metric
  tokens[, rIP := rarity / sum(rarity), by = .(get(id), column)]
  
  # --- 4. Self-join on shared tokens (within blocks) -----------------------
  
  block_by <- strategy@block_by %||% character()
  by_cols  <- c("column", "token", block_by)
  
  rhs <- tokens[, c(id, "row_id", "column", "token", block_by), with = FALSE]
  
  id2  <- paste0(id, "_2")
  row2 <- "row_id_2"
  
  data.table::setnames(rhs, id,  id2)
  data.table::setnames(rhs, "row_id", row2)
  
  joined <- tokens[
    rhs,
    on = by_cols,
    allow.cartesian = TRUE,
    nomatch = 0
  ]
  
  # Remove self matches
  joined <- joined[joined[[id]] != joined[[id2]]]
  
  scored <- joined[
    , .(score = sum(rIP * weight, na.rm = TRUE)),
    by = c(id, id2)
  ]
  
  # Apply threshold
  scored <- scored[score >= threshold]
  
  if (nrow(scored) == 0L) {
    return(data.table(
      duplicate_group = integer(),
      id              = character(),
      score           = numeric(),
      rank            = integer()
    ))
  }
  
  # --- 6. Build connected components (robust edges) ------------------------
  edges <- scored[, .(
    from = .SD[[1]],
    to   = .SD[[2]]
  ), .SDcols = c(id, id2)]
  
  
  # Mirror edges for safety
  edges <- rbind(edges, edges[, .(from = to, to = from)])
  
  # Ensure all nodes included
  all_ids <- unique(c(tokens[[id]]))
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = all_ids)
  comp <- igraph::components(g)
  
  membership_dt <- data.table(
    id              = names(comp$membership),
    duplicate_group = unname(comp$membership)
  )
  
  # --- 7. Insert scores & ranks -------------------------------------------
  scored_long <- rbindlist(list(
    scored[, .(id = get(id),  score)],
    scored[, .(id = get(id2), score)]
  ))
  
  best <- scored_long[
    , .(score = max(score, na.rm = TRUE)),
    by = id
  ]
  
  result <- membership_dt[best, on = "id"]
  
  result[, rank := rank(-score, ties.method = "first"), by = duplicate_group]
  
  data.table::setkeyv(result, c("duplicate_group", "rank"))
  
  # ---8. Attach original data -----------------------------------------------
  result <- merge(
    result,
    dt,
    by.x = "id",
    by.y = id,
    all.x = TRUE,
    sort = FALSE
  )
  
  result[]
}

# Method: deduplicate_table 
#------------------------------------------------------------------------------
method(
  deduplicate_table,
  list(DT_tbl, DT_tbl, class_character)
) <- function(base_table, duplicates, id) {
  dt <- data.table::copy(base_table)
  
  if (!id %in% names(dt)) {
    stop(sprintf("ID '%s' not found in base_table", col), call. = FALSE)
  }      
  duplicate_ids <- duplicates[rank!=1L,]$id
  to_remove <- dt[[id]] %in% duplicate_ids
  
  dt[!to_remove,][]
}


# Method: search_candidates 
#------------------------------------------------------------------------------
method(
  search_candidates,
  list(DT_tbl, DT_tbl, class_character, class_character, Search_Strategy)
) <- function(base_table,
              target_table,
              base_id,
              target_id,
              strategy,
              threshold,
              weights = NULL) {

  # --- 0. Copy inputs -------------------------------------------------------
  base_dt   <- data.table::copy(base_table)
  target_dt <- data.table::copy(target_table)
  
  block_by <- strategy@block_by %||% character()
  
  # --- 1. Prepare token tables ----------------------------------------------
  base_tokens <- prepare_search_data(base_dt,   base_id,   strategy)
  base_tokens[, side := "base"]
  
  target_tokens <- prepare_search_data(target_dt, target_id, strategy)
  target_tokens[, side := "target"]
  
  # Add a unified key for side-specific IDs
  base_tokens[, uid := base_tokens[[base_id]]]
  target_tokens[, uid := target_tokens[[target_id]]]
  
  # --- 2. Compute rarity -----------------------------------------------------
  all_tokens <- rbindlist(list(base_tokens, target_tokens), use.names = TRUE, fill = TRUE)
  all_tokens <- compute_rarity(all_tokens, strategy)
  
  # Split back
  base_tokens   <- all_tokens[side == "base"]
  target_tokens <- all_tokens[side == "target"]
  
  # --- 3. Determine column weights ------------------------------------------
  if (is.null(weights)) {
    if (length(strategy@weights) > 0) {
      weights <- strategy@weights
    } else {
      cols <- names(strategy@preparers)
      weights <- rep(1 / length(cols), length(cols))
      names(weights) <- cols
    }
  }
  
  missing_w <- setdiff(unique(all_tokens$column), names(weights))
  if (length(missing_w) > 0) {
    stop("Weights missing for columns: ", paste(missing_w, collapse = ", "))
  }
  
  base_tokens[, weight := weights[column]]
  target_tokens[, weight := weights[column]]
  
  # --- 4. Compute rIP per record × column -----------------------------------
  base_tokens[,  rIP := rarity / sum(rarity), by = .(uid, column)]
  target_tokens[, rIP := rarity / sum(rarity), by = .(uid, column)]
  
  # --- 5. Cross-table join on shared tokens (respecting block_by) ------------
  by_cols <- c("column", "token", block_by)
  
  rhs <- target_tokens[, c("uid", "row_id", "column", "token", block_by), with = FALSE]
  data.table::setnames(rhs, "uid",    "uid2")
  data.table::setnames(rhs, "row_id", "row_id2")
  
  joined <- base_tokens[
    rhs,
    on = by_cols,
    allow.cartesian = TRUE,
    nomatch = 0L
  ]
  
  # --- 6. Compute pairwise similarity ----------------------------------------
  scored <- joined[
    , .(score = sum(rIP * weight, na.rm = TRUE)),
    by = .(uid, uid2)
  ]
  
  scored <- scored[score >= threshold]
  
  if (nrow(scored) == 0) {
    return(data.table(
      match_id = integer(),
      score    = numeric(),
      source   = character(),
      id       = character(),
      rank     = integer()
    ))
  }
  
  # --- 7. Assign match IDs ---------------------------------------------------
  scored[, match_id := .I]
  
  # --- 8. Expand to long form (base + target rows per match) -----------------
  long <- rbindlist(list(
    scored[, .(match_id, score, source = "base",   id = uid)],
    scored[, .(match_id, score, source = "target", id = uid2)]
  ))
  
  # --- 9. Attach original base and target metadata ---------------------------
  base_dt2   <- data.table::copy(base_dt)
  target_dt2 <- data.table::copy(target_dt)
  
  base_dt2[, id := base_dt2[[base_id]]]
  target_dt2[, id := target_dt2[[target_id]]]
  
  base_long <- merge(
    long[source == "base"],
    base_dt2,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )
  
  target_long <- merge(
    long[source == "target"],
    target_dt2,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )
  
  out <- rbindlist(list(base_long, target_long), use.names = TRUE, fill = TRUE)
  
  # --- 10. Rank within match_id ---------------------------------------------
  out[, rank := rank(-score, ties.method = "first"), by = match_id]
  
  # Standardize ordering
  data.table::setorder(out, match_id, source, rank)
  
  out[]
}





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
