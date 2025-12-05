# ============================================================
# data.frame / tibble wrappers for match statistics
# ============================================================
#
# Provides S7 method implementations that enable match statistics
# to operate on base data.frames and tibbles by delegating to the
# data.table backend.
#
# Workflow:
#   1. Convert incoming data.frame / tibble to data.table via as_DT()
#   2. Call the DT_tbl method implementation
#   3. Convert results back via back_to_original()
#
# No statistical logic lives here - this file only provides thin
# compatibility layers.
#
# ============================================================


# ============================================================
# Phase 1: Core Match Summaries
# ============================================================

# Method: summarize_matches (data.frame)
#------------------------------------------------------------------------------
#method(
#  summarize_matches,
#  list(.jyDF)
#) <- function(matches, type = NULL, base_id = NULL, target_id = NULL) {
#  summarize_matches(as_DT(matches), type = type, base_id = base_id, 
#                    target_id = target_id)
#}
#
#method(
#  summarize_matches,
#  list(.jyTBL_DF)
#) <- function(matches, type = NULL, base_id = NULL, target_id = NULL) {
#  summarize_matches(as_DT(matches), type = type, base_id = base_id, 
#                    target_id = target_id)
#}
#
#
## Method: score_distribution (data.frame)
##------------------------------------------------------------------------------
#method(
#  score_distribution,
#  list(.jyDF)
#) <- function(matches, breaks = 10, plot = FALSE) {
#  result <- score_distribution(as_DT(matches), breaks = breaks, plot = plot)
#  if (!plot) {
#    back_to_original(result, matches)
#  } else {
#    result  # plot objects returned as-is
#  }
#}
#
#method(
#  score_distribution,
#  list(.jyTBL_DF)
#) <- function(matches, breaks = 10, plot = FALSE) {
#  result <- score_distribution(as_DT(matches), breaks = breaks, plot = plot)
#  if (!plot) {
#    back_to_original(result, matches)
#  } else {
#    result
#  }
#}
#
#
## Method: sample_matches (data.frame)
##------------------------------------------------------------------------------
#method(
#  sample_matches,
#  list(.jyDF)
#) <- function(matches, n = 10, type = c("high", "low", "borderline", "random"),
#                threshold = NULL) {
#  result <- sample_matches(as_DT(matches), n = n, type = type, 
#                           threshold = threshold)
#  back_to_original(result, matches)
#}
#
#method(
#  sample_matches,
#  list(.jyTBL_DF)
#) <- function(matches, n = 10, type = c("high", "low", "borderline", "random"),
#                threshold = NULL) {
#  result <- sample_matches(as_DT(matches), n = n, type = type, 
#                           threshold = threshold)
#  back_to_original(result, matches)
#}
#