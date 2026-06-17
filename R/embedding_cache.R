# ============================================================
# Embedding reuse cache for joinery
# ============================================================
#
# Content-addressed cache of raw (pre-normalization) embedding vectors so the
# in-memory backends do not re-bill embedding generation on every call. Mirrors
# the reuse the DuckDB backend gets from its persisted `embeddings` column.
#
# Vectors are keyed by (model, content-hash of the assembled record text), never
# by id alone, so a record whose text changed is re-embedded automatically.
# Raw (pre-normalize) vectors are stored, so `normalize = TRUE` and
# `normalize = FALSE` reads share the same cache; normalization is applied on read.
#
# Two tiers: a session environment (always on) and an opt-in on-disk directory
# enabled via `options(joinery.embedding_cache_dir = <path>)`. Lookup order is
# session env -> disk (hydrated into the env) -> miss.
# ============================================================


# Session store. Parent is emptyenv() so lookups never fall through to globals.
.embedding_cache <- new.env(parent = emptyenv())


#' Is embedding reuse enabled?
#'
#' Reuse is on by default. Set `options(joinery.embedding_reuse = FALSE)` to make
#' every call re-embed (e.g. for benchmarking, or with a non-deterministic model).
#' @noRd
.embedding_reuse_enabled <- function() {
  isTRUE(getOption("joinery.embedding_reuse", default = TRUE))
}


#' Stable cache key for an embedding model object
#'
#' @param model A tidyllm provider object (or `NULL` in tests).
#' @return A scalar hash string.
#' @noRd
.embedding_model_key <- function(model) {
  rlang::hash(model)
}


#' Per-record cache keys
#'
#' @param text Character vector of assembled record texts.
#' @param model_key The model key from `.embedding_model_key()`.
#' @return Character vector of keys, one per element of `text`. Keys use only
#'   hex characters and an underscore, so they are safe as filenames on every
#'   platform (the on-disk tier writes `<key>.rds`).
#' @noRd
.embedding_keys <- function(text, model_key) {
  map_chr(text, function(t) paste0(model_key, "_", rlang::hash(t)))
}


#' Current on-disk cache directory, or NULL for session-only
#'
#' @return A directory path, or `NULL` when disk caching is disabled.
#' @noRd
.embedding_cache_dir <- function() {
  getOption("joinery.embedding_cache_dir", default = NULL)
}


#' Disk path for one cache key
#'
#' @noRd
.embedding_cache_path <- function(dir, key) {
  file.path(dir, paste0(key, ".rds"))
}


#' Look up raw vectors for a set of keys
#'
#' Checks the session env first; on a miss, checks the on-disk cache (when
#' enabled) and hydrates the env so later lookups in the same session are fast.
#'
#' @param keys Character vector of cache keys.
#' @return A list the same length as `keys`; each element is a raw numeric vector
#'   or `NULL` when not cached.
#' @noRd
.embedding_cache_get <- function(keys) {
  if (!.embedding_reuse_enabled()) {
    return(vector("list", length(keys))) # all NULL -> all misses
  }
  dir <- .embedding_cache_dir()

  lapply(keys, function(key) {
    if (exists(key, envir = .embedding_cache, inherits = FALSE)) {
      return(get(key, envir = .embedding_cache, inherits = FALSE))
    }
    if (!is.null(dir)) {
      path <- .embedding_cache_path(dir, key)
      if (file.exists(path)) {
        vec <- readRDS(path)
        assign(key, vec, envir = .embedding_cache)
        return(vec)
      }
    }
    NULL
  })
}


#' Store raw vectors for a set of keys
#'
#' Writes the session env always, and the on-disk cache when enabled.
#'
#' @param keys Character vector of cache keys.
#' @param vecs A list of raw numeric vectors, parallel to `keys`.
#' @return Invisibly `NULL`.
#' @noRd
.embedding_cache_put <- function(keys, vecs) {
  if (!.embedding_reuse_enabled()) {
    return(invisible(NULL))
  }
  dir <- .embedding_cache_dir()
  if (!is.null(dir) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_along(keys)) {
    key <- keys[[i]]
    vec <- vecs[[i]]
    assign(key, vec, envir = .embedding_cache)
    if (!is.null(dir)) {
      saveRDS(vec, .embedding_cache_path(dir, key))
    }
  }
  invisible(NULL)
}


#' Clear the embedding reuse cache
#'
#' @description
#' Empties joinery's in-session embedding cache, and optionally the on-disk
#' cache. The cache stores raw embedding vectors so that the data.table and
#' tibble backends reuse them instead of re-embedding on every call. You rarely
#' need to call this by hand; it is mainly useful to force a clean re-embed or to
#' reclaim memory in a long-running session.
#'
#' @param disk Logical. If `TRUE`, also delete the on-disk cache files in the
#'   directory set by `options(joinery.embedding_cache_dir = ...)`. Defaults to
#'   `FALSE` (clear the in-session cache only).
#'
#' @return Invisibly `NULL`.
#'
#' @seealso [compute_embeddings()] for how the cache is filled, and
#'   `joinery` (package options) for `joinery.embedding_reuse` and
#'   `joinery.embedding_cache_dir`.
#'
#' @export
clear_embedding_cache <- function(disk = FALSE) {
  rm(
    list = ls(envir = .embedding_cache, all.names = TRUE),
    envir = .embedding_cache
  )

  if (isTRUE(disk)) {
    dir <- .embedding_cache_dir()
    if (!is.null(dir) && dir.exists(dir)) {
      files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
      if (length(files) > 0L) file.remove(files)
    }
  }

  invisible(NULL)
}
