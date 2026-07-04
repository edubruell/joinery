# ============================================================
# Subword tokenization: find_subwords() + subword_tokens()
# ============================================================
#
# The fit/apply pair for learned subword vocabularies, backed by the
# sentencepiece package (Suggests). find_subwords() trains a vocabulary on a
# text corpus and returns a fitted Subword_Model; subword_tokens() applies it
# inside a strategy formula through the tokenize() protocol.
#
# The fitted model stores the raw bytes of the trained SentencePiece model
# file, never a file path or the loaded external pointer, so it survives
# saveRDS() and (later) strategy serialization. The pointer is rehydrated
# lazily and memoised per session in .sp_cache.
# ============================================================


#' Abort unless the sentencepiece package is installed
#' @noRd
.check_sentencepiece <- function(call = rlang::caller_env()) {
  if (!requireNamespace("sentencepiece", quietly = TRUE)) {
    cli::cli_abort(c(
      "Subword tokenization requires the {.pkg sentencepiece} package.",
      "i" = "Install it with {.code install.packages('sentencepiece')}."
    ), call = call)
  }
}


# ---------------------------------------------------------------------------
# Subword_Model class
# ---------------------------------------------------------------------------

#' Subword_Model Class
#'
#' @description
#' A fitted subword vocabulary returned by [find_subwords()]. Carries the raw
#' bytes of the trained SentencePiece model (never a file path or a loaded
#' pointer), the algorithm and vocabulary it was fitted with, and the
#' boundary-marker setting the apply step must honour.
#'
#' @slot model_bytes Raw vector holding the trained model file.
#' @slot algorithm `"bpe"` or `"unigram"`.
#' @slot vocab_size The fitted vocabulary size (may be smaller than requested
#'   on a small corpus).
#' @slot vocabulary Data frame of the fitted vocabulary (`id`, `subword`).
#' @slot keep_boundary Whether word-boundary markers stay on the tokens.
#'
#' @noRd
Subword_Model <- new_class("Subword_Model",
  properties = list(
    model_bytes   = class_raw,
    algorithm     = class_character,
    vocab_size    = class_integer,
    vocabulary    = class_data.frame,
    keep_boundary = class_logical
  ),
  validator = function(self) {
    if (length(self@algorithm) != 1L ||
        !self@algorithm %in% c("bpe", "unigram")) {
      "@algorithm must be \"bpe\" or \"unigram\""
    } else if (length(self@model_bytes) == 0L) {
      "@model_bytes must not be empty"
    } else if (length(self@keep_boundary) != 1L || is.na(self@keep_boundary)) {
      "@keep_boundary must be TRUE or FALSE"
    } else {
      NULL
    }
  }
)

#' @noRd
print.Subword_Model <- new_external_generic("base", "print", "x")

#' @noRd
method(print.Subword_Model, Subword_Model) <- function(x, ...) {
  cat("<joinery::Subword_Model>\n")
  cat("  Algorithm:  ", x@algorithm, "\n", sep = "")
  cat("  Vocabulary: ", x@vocab_size, " subwords\n", sep = "")
  cat("  Boundary markers: ", if (x@keep_boundary) "kept" else "stripped",
      "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# Session cache: raw model bytes -> loaded SentencePiece model
# ---------------------------------------------------------------------------

.sp_cache <- new.env(parent = emptyenv())

#' Rehydrate the SentencePiece model from its stored bytes (memoised)
#'
#' The loaded model is an external pointer that cannot be stored on the S7
#' object (it would not survive saveRDS). Instead the bytes are written to a
#' temporary file and loaded on first use, then cached for the session keyed
#' by a hash of the bytes, so repeated prepare calls (and DuckDB batches) pay
#' the load once.
#' @noRd
.sp_load <- function(model) {
  key <- rlang::hash(model@model_bytes)
  hit <- get0(key, envir = .sp_cache, inherits = FALSE)
  if (!is.null(hit)) return(hit)

  tmp <- tempfile(fileext = ".model")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(model@model_bytes, tmp)
  loaded <- sentencepiece::sentencepiece_load_model(tmp)
  assign(key, loaded, envir = .sp_cache)
  loaded
}


# ---------------------------------------------------------------------------
# Fit: find_subwords()
# ---------------------------------------------------------------------------

#' Learn a subword vocabulary from your text
#'
#' @description
#' Word tokens fail quietly on compound words and misspellings:
#' `"Maschinenbaugesellschaft"` and `"Maschinenbau GmbH"` share no word, so
#' they never meet in a word-token match. Subword tokens fix this by learning,
#' from your own data, a vocabulary of frequent word pieces and splitting every
#' record into those pieces. Two spellings of the same name then share most of
#' their pieces even when they share no whole word.
#'
#' `find_subwords()` is the learning step. Run it once on the text of a column
#' (after the same cleaning you use in your strategy), then place the fitted
#' model in the strategy formula with [subword_tokens()]:
#'
#' ```r
#' sw <- find_subwords(normalize_text(register$name))
#' strat <- search_strategy(
#'   name ~ normalize_text() + subword_tokens(sw)
#' )
#' ```
#'
#' The fitted model is self-contained: you can save it with `saveRDS()` and
#' reuse it in a later session, and the same model always splits text the same
#' way.
#'
#' @details
#' Fit the model on the text as the strategy will see it: apply the same
#' cleaning steps (for example [normalize_text()]) to the training text that
#' precede `subword_tokens()` in the formula. Training on raw text and
#' applying to cleaned text (or the other way round) degrades the splits.
#'
#' `vocab_size` sets how fine the splits are. Small vocabularies split
#' aggressively into short pieces; large ones keep frequent words whole and
#' only split rare ones. The default of 500 suits mid-sized name corpora;
#' large corpora often support 1000 to 8000. On a very small corpus the
#' fitted vocabulary can come out smaller than requested; the model records
#' the size actually fitted. [rarity_distribution()] on the prepared tokens
#' is the read for judging the result.
#'
#' Very frequent single-character pieces are normal and mostly harmless: they
#' carry almost no rarity, so they contribute little to a score. Compose
#' `subword_tokens(sw) + drop_short_tokens(2)` to drop them outright.
#'
#' @param x The training text: a character vector, or a table (data frame,
#'   tibble, data.table, DuckDB table) together with `columns`.
#' @param vocab_size Number of subword pieces to learn. Default `500`.
#' @param algorithm `"bpe"` (default) or `"unigram"`, the two learning
#'   algorithms SentencePiece offers. Start with `"bpe"`; try `"unigram"` if
#'   its splits look better on your data.
#' @param keep_boundary Keep the word-boundary marker on each piece? The
#'   default `FALSE` strips it, so the same piece matches whether it starts a
#'   word or not; that is usually what record linkage wants.
#' @param sample_n Optional cap on the number of rows used for training. On a
#'   corpus of millions of rows a uniform sample of a few hundred thousand
#'   fits an equivalent vocabulary in a fraction of the time.
#' @param columns For table input: character vector naming the text column(s)
#'   to train on.
#'
#' @return A fitted `Subword_Model`, ready for [subword_tokens()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sentencepiece", quietly = TRUE)) {
#'   firms <- rep(c(
#'     "maschinenbau mueller", "tischlerei schmidt",
#'     "holzbau wagner", "metallbau krause"
#'   ), 40)
#'   sw <- find_subwords(firms, vocab_size = 60)
#'   sw
#'   subword_tokens("maschinenbaugesellschaft mueller", sw)
#' }
#' }
#'
#' @seealso [subword_tokens()] to use the model in a strategy;
#'   [find_stopwords()] for the same fit-then-apply pattern on stopwords.
#' @export
find_subwords <- new_generic(
  "find_subwords", "x",
  function(x, vocab_size = 500L, algorithm = c("bpe", "unigram"),
           keep_boundary = FALSE, sample_n = NULL, columns = NULL) {
    S7_dispatch()
  }
)


#' Validate the shared find_subwords() knobs
#' @noRd
.find_subwords_validate <- function(vocab_size, keep_boundary, sample_n,
                                    call = rlang::caller_env()) {
  check_number_whole(vocab_size, min = 10, call = call)
  check_bool(keep_boundary, call = call)
  check_number_whole(sample_n, min = 1, allow_null = TRUE, call = call)
  invisible(NULL)
}


#' Train a SentencePiece model on a text file already on disk
#'
#' The shared trainer every find_subwords() method funnels into. Trains into
#' a temporary directory, reads the model file back as raw bytes, and cleans
#' both up; only the bytes leave this function.
#' @noRd
.sp_fit_file <- function(path, vocab_size, algorithm, keep_boundary,
                         call = rlang::caller_env()) {
  model_dir <- tempfile("joinery_sp_")
  dir.create(model_dir)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  fit <- tryCatch(
    sentencepiece::sentencepiece(
      path, type = algorithm, vocab_size = as.integer(vocab_size),
      model_dir = model_dir, verbose = FALSE
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Training the subword vocabulary failed.",
        "x" = conditionMessage(e),
        "i" = "A lower {.arg vocab_size} or more training text usually fixes this."
      ), call = call)
    }
  )

  bytes <- readBin(fit$model_path, what = "raw",
                   n = file.info(fit$model_path)$size)
  Subword_Model(
    model_bytes   = bytes,
    algorithm     = algorithm,
    vocab_size    = as.integer(fit$vocab_size),
    vocabulary    = fit$vocabulary,
    keep_boundary = keep_boundary
  )
}


# Method: find_subwords for a character vector (the workhorse)
#------------------------------------------------------------------------------
method(find_subwords, class_character) <- function(
    x, vocab_size = 500L, algorithm = c("bpe", "unigram"),
    keep_boundary = FALSE, sample_n = NULL, columns = NULL) {

  .check_sentencepiece()
  algorithm <- rlang::arg_match(algorithm)
  .find_subwords_validate(vocab_size, keep_boundary, sample_n)
  if (!is.null(columns)) {
    cli::cli_abort(c(
      "{.arg columns} only applies to table input.",
      "i" = "You already passed the text itself; drop {.arg columns}."
    ))
  }

  txt <- x[!is.na(x) & nzchar(x)]
  if (!length(txt)) {
    cli::cli_abort("{.arg x} contains no non-empty text to train on.")
  }
  if (!is.null(sample_n) && length(txt) > sample_n) {
    txt <- sample(txt, size = as.integer(sample_n))
  }

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(enc2utf8(txt), tmp, useBytes = TRUE)

  .sp_fit_file(tmp, vocab_size, algorithm, keep_boundary)
}


# Method: find_subwords for data.frame / tibble / data.table input
#------------------------------------------------------------------------------
method(find_subwords, new_S3_class("data.frame")) <- function(
    x, vocab_size = 500L, algorithm = c("bpe", "unigram"),
    keep_boundary = FALSE, sample_n = NULL, columns = NULL) {

  columns <- .find_subwords_columns(columns, colnames(x))
  txt <- unlist(
    lapply(columns, function(cl) as.character(x[[cl]])),
    use.names = FALSE
  )
  find_subwords(txt, vocab_size = vocab_size, algorithm = algorithm,
                keep_boundary = keep_boundary, sample_n = sample_n)
}


# Method: find_subwords for a DuckDB table (streams text out in batches)
#------------------------------------------------------------------------------
# No file-load guard needed: a tbl_duckdb_connection input already implies
# duckdb / DBI / dplyr / dbplyr are available (same reasoning as the
# find_stopwords DuckDB method).
method(find_subwords, new_S3_class("tbl_duckdb_connection")) <- function(
    x, vocab_size = 500L, algorithm = c("bpe", "unigram"),
    keep_boundary = FALSE, sample_n = NULL, columns = NULL) {

  .check_sentencepiece()
  algorithm <- rlang::arg_match(algorithm)
  .find_subwords_validate(vocab_size, keep_boundary, sample_n)
  columns <- .find_subwords_columns(columns, dplyr::tbl_vars(x))

  con <- x$src$con
  sql <- dbplyr::sql_render(dplyr::select(x, dplyr::all_of(columns)))
  if (!is.null(sample_n)) {
    sql <- paste0(
      "SELECT * FROM (", sql, ") USING SAMPLE reservoir(",
      as.integer(sample_n), " ROWS)"
    )
  }

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)

  res <- DBI::dbSendQuery(con, sql)
  out_con <- NULL
  n_lines <- 0L
  tryCatch({
    out_con <- file(tmp, open = "w", encoding = "UTF-8")
    while (!DBI::dbHasCompleted(res)) {
      chunk <- DBI::dbFetch(res, n = 500000L)
      # Multi-column input interleaves the columns per fetch chunk here,
      # while the in-memory method concatenates whole columns, so training
      # lines arrive in a different order between the backends. SentencePiece
      # training is order-sensitive in principle; single-column fits are
      # byte-identical across backends (tested), multi-column fits may not be.
      for (cl in names(chunk)) {
        v <- as.character(chunk[[cl]])
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v)) {
          writeLines(enc2utf8(v), out_con, useBytes = TRUE)
          n_lines <- n_lines + length(v)
        }
      }
    }
  }, finally = {
    DBI::dbClearResult(res)
    if (!is.null(out_con)) close(out_con)
  })

  if (n_lines == 0L) {
    cli::cli_abort("Column{?s} {.field {columns}} contain{?s/} no non-empty text to train on.")
  }

  .sp_fit_file(tmp, vocab_size, algorithm, keep_boundary)
}


#' Validate the columns argument for table input
#' @noRd
.find_subwords_columns <- function(columns, available,
                                   call = rlang::caller_env()) {
  if (is.null(columns) || !is.character(columns) || !length(columns)) {
    cli::cli_abort(c(
      "{.arg columns} must name the text columns to train on.",
      "i" = "For example {.code find_subwords(data, columns = \"name\")}."
    ), call = call)
  }
  missing <- setdiff(columns, available)
  if (length(missing)) {
    cli::cli_abort("Column{?s} not found in {.arg x}: {.field {missing}}.",
                   call = call)
  }
  columns
}


# ---------------------------------------------------------------------------
# Apply: tokenize() method + subword_tokens()
# ---------------------------------------------------------------------------

#' @noRd
method(tokenize, Subword_Model) <- function(tokenizer, text, ...) {
  .check_sentencepiece()
  check_character(text)

  # sentencepiece encodes NA as the literal string "NA"; blank it first so an
  # NA record yields no tokens, like every other tokenizer in joinery.
  txt <- text
  txt[is.na(txt)] <- ""

  loaded <- .sp_load(tokenizer)
  pieces <- sentencepiece::sentencepiece_encode(loaded, txt, type = "subwords")

  if (!tokenizer@keep_boundary) {
    # Strip the word-boundary marker (U+2581) flat and vectorised, mirroring
    # the flatten / filter / re-split idiom of word_tokens(). Pieces that were
    # only the marker become empty and drop.
    lens <- lengths(pieces)
    flat <- unlist(pieces, use.names = FALSE)
    if (is.null(flat)) flat <- character(0)
    flat <- stringi::stri_replace_all_fixed(flat, "\u2581", "")
    keep <- nzchar(flat)
    idx  <- rep.int(seq_along(pieces), lens)[keep]
    pieces <- split(flat[keep], factor(idx, levels = seq_along(pieces)))
    names(pieces) <- NULL
  }

  pieces
}


#' Split text into learned subword tokens
#'
#' The apply half of the subword pair: place a model fitted by
#' [find_subwords()] in a strategy formula and every record is split into the
#' learned word pieces:
#' `name ~ normalize_text() + subword_tokens(sw)`.
#'
#' Use it where word tokens are too brittle: compound words
#' (`"Maschinenbaugesellschaft"` never word-matches `"Maschinenbau"`),
#' frequent misspellings, or OCR noise. Two spellings of the same name share
#' most of their pieces even when they share no whole word, so they can still
#' meet and score. Scoring is unchanged: subword pieces are ordinary tokens,
#' each weighted by its own rarity.
#'
#' The model decides how text is split, so pass the text through the same
#' cleaning steps the model was fitted on (see [find_subwords()]).
#'
#' @param text A character vector to tokenize.
#' @param model A fitted `Subword_Model` from [find_subwords()].
#'
#' @return A list of character vectors, one per input element, each holding
#'   that element's subword tokens.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sentencepiece", quietly = TRUE)) {
#'   firms <- rep(c(
#'     "maschinenbau mueller", "tischlerei schmidt",
#'     "holzbau wagner", "metallbau krause"
#'   ), 40)
#'   sw <- find_subwords(firms, vocab_size = 60)
#'   subword_tokens(c("maschinenbaugesellschaft", "holzbau wagner kg"), sw)
#' }
#' }
#'
#' @family token generators
#' @seealso [find_subwords()] to fit the model; [custom_tokens()], which this
#'   is a named shortcut for; [drop_short_tokens()] to drop single-letter
#'   pieces.
#' @export
subword_tokens <- function(text, model) {
  if (!S7_inherits(model, Subword_Model)) {
    cli::cli_abort(c(
      "{.arg model} must be a fitted {.cls Subword_Model}.",
      "i" = "Fit one with {.fn find_subwords}."
    ))
  }
  custom_tokens(text, model)
}
