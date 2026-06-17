# ============================================================
# S7 generics — embedding-based matching (tidyllm-dependent)
# ============================================================

#' Compute Embeddings for Records
#'
#' @description
#' Compute embedding vectors for records using an `Embedding_Strategy`.
#' This is a backend-specific generic that handles data retrieval,
#' text assembly, and embedding computation via tidyllm.
#'
#' Embedding is the expensive part of a vector match, so each record is embedded
#' once and the vector is reused on later calls. The data.table and tibble
#' backends keep a per-session cache keyed by model and record content; the
#' DuckDB backend reuses through its persisted `embeddings` column. Reuse is
#' controlled by the `joinery.embedding_reuse` and `joinery.embedding_cache_dir`
#' options (see `joinery` package options) and can be cleared with
#' [clear_embedding_cache()].
#'
#' @param data A data.frame / tibble / data.table (or db table in other backends).
#' @param id Character scalar naming the ID column in `data`.
#' @param strategy An `Embedding_Strategy` object specifying columns,
#'   embedding model, and normalization settings.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A backend-specific table with columns: `id` and `embedding`
#'   (where `embedding` contains numeric vectors).
#'
#' @seealso [clear_embedding_cache()]
#'
#' @export
compute_embeddings <- new_generic(
  "compute_embeddings",
  c("data", "id", "strategy")
)

#' Score Embedding Pairs Using Cosine Similarity
#'
#' @description
#' Compute cosine similarity scores between base and target embeddings.
#' This is a pure scoring function that operates on pre-computed embeddings.
#'
#' @param base_embeddings A table with columns: `id` and `embedding`.
#' @param target_embeddings A table with columns: `id` and `embedding`.
#' @param strategy An `Embedding_Strategy` object (used for normalization settings).
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A backend-specific table with columns: `base_id`, `target_id`, `score`.
#'
#' @export
score_embeddings <- new_generic(
  "score_embeddings",
  c("base_embeddings", "target_embeddings", "strategy")
)
