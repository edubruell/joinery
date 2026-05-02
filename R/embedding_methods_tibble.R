if (requireNamespace("tidyllm", quietly = TRUE)) {

# compute_embeddings
#--------------------------------------------------------------------------

method(
  compute_embeddings,
  list(.jyDF, class_character, Embedding_Strategy)
) <- function(data, id, strategy) {
  out <- compute_embeddings(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  compute_embeddings,
  list(.jyTBL_DF, class_character, Embedding_Strategy)
) <- function(data, id, strategy) {
  out <- compute_embeddings(as_DT(data), id, strategy)
  back_to_original(out, data)
}

method(
  compute_embeddings,
  list(.jyTBL, class_character, Embedding_Strategy)
) <- function(data, id, strategy) {
  out <- compute_embeddings(as_DT(data), id, strategy)
  back_to_original(out, data)
}

# score_embeddings
#--------------------------------------------------------------------------

method(
  score_embeddings,
  list(.jyDF, .jyDF, Embedding_Strategy)
) <- function(base_embeddings, target_embeddings, strategy) {
  out <- score_embeddings(as_DT(base_embeddings), as_DT(target_embeddings), strategy)
  back_to_original(out, base_embeddings)
}

method(
  score_embeddings,
  list(.jyTBL_DF, .jyTBL_DF, Embedding_Strategy)
) <- function(base_embeddings, target_embeddings, strategy) {
  out <- score_embeddings(as_DT(base_embeddings), as_DT(target_embeddings), strategy)
  back_to_original(out, base_embeddings)
}

method(
  score_embeddings,
  list(.jyTBL, .jyTBL, Embedding_Strategy)
) <- function(base_embeddings, target_embeddings, strategy) {
  out <- score_embeddings(as_DT(base_embeddings), as_DT(target_embeddings), strategy)
  back_to_original(out, base_embeddings)
}

# search_candidates with Embedding_Strategy
#--------------------------------------------------------------------------

method(
  search_candidates,
  list(.jyDF, .jyDF, class_character, class_character, Embedding_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, threshold = NULL, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, threshold, weights)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL_DF, .jyTBL_DF, class_character, class_character, Embedding_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, threshold = NULL, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, threshold, weights)
  back_to_original(out, base_table)
}

method(
  search_candidates,
  list(.jyTBL, .jyTBL, class_character, class_character, Embedding_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, threshold = NULL, weights = NULL) {
  out <- search_candidates(as_DT(base_table), as_DT(target_table), base_id, target_id, strategy, threshold, weights)
  back_to_original(out, base_table)
}

# detect_duplicates with Embedding_Strategy
#--------------------------------------------------------------------------

method(
  detect_duplicates,
  list(.jyDF, class_character, Embedding_Strategy)
) <- function(base_table, id, strategy, threshold = NULL) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, threshold)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL_DF, class_character, Embedding_Strategy)
) <- function(base_table, id, strategy, threshold = NULL) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, threshold)
  back_to_original(out, base_table)
}

method(
  detect_duplicates,
  list(.jyTBL, class_character, Embedding_Strategy)
) <- function(base_table, id, strategy, threshold = NULL) {
  out <- detect_duplicates(as_DT(base_table), id, strategy, threshold)
  back_to_original(out, base_table)
}

} # end if (requireNamespace("tidyllm"))
