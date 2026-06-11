# ============================================================
# S7 generics — core token-matching workflow
# ============================================================
#
# Defines joinery's core token-matching functions as S7 generics
# that get backend methods via multiple dispatch.
#
# ============================================================

#' Prepare Data for Record Linkage Search
#'
#' @param data A data.frame / tibble / data.table (or db table in other backends).
#' @param id   Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @export
prepare_search_data <- new_generic(
  "prepare_search_data",
  c("data", "id", "strategy")
)

#' Detect Duplicate Records
#'
#' @description
#' Identify likely duplicate records within a single table using
#' token-based similarity scoring defined in a `Search_Strategy`.
#'
#' Backends must:
#' - preprocess data using `prepare_search_data()`,
#' - compute token rarity using the strategy's rarity method,
#' - join records on shared tokens (respecting `block_by`),
#' - aggregate rarity × column-weight contributions into a similarity score,
#' - return only pairs with `score >= threshold`,
#' - group connected pairs into duplicate clusters.
#'
#' @param base_table A data.frame, tibble, data.table, or backend-specific
#'   table to deduplicate.
#' @param id Character scalar naming the ID column in `base_table`.
#' @param strategy A `Search_Strategy` object defining preprocessing steps,
#'   blocking variables, rarity metric, and optional column weights.
#' @param ... Additional arguments passed to backend-specific methods,
#'   including `threshold` (minimum similarity score) and `weights`
#'   (named numeric vector overriding strategy weights).
#'
#' @return A backend-specific table containing at least:
#' \describe{
#'   \item{duplicate_group}{Integer cluster label.}
#'   \item{id}{Record ID.}
#'   \item{score}{Similarity score for the record within its cluster.}
#'   \item{rank}{Rank of the record within its duplicate group.}
#'   \item{<original columns>}{All additional columns from `base_table`.}
#' }
#'
#' @export
detect_duplicates <- new_generic(
  "detect_duplicates",
  c("base_table", "id", "strategy")
)

#' Resolve an Edge List into Entities
#'
#' @description
#' Group a list of record-pair edges into entities via connected
#' components, assign each vertex a within-entity rank, and mark a canonical
#' representative. This is the shared entity-resolution kernel underlying
#' [detect_duplicates()]; it is exposed so that *any* edge list — not only a
#' `detect_duplicates()` self-join — can be resolved into entity ids.
#'
#' @details
#' Output is deterministic: given identical `edges`, the returned
#' `entity` / `rep` / `rank` are byte-identical regardless of edge row order.
#' `entity` is a dense integer label assigned in ascending order of each
#' component's smallest member id (its *root*). The representative (`rep`,
#' the rank-1 member) is chosen by: descending best `score` (when supplied),
#' then ascending `rep_by` (when supplied), then ascending `id`.
#'
#' @param edges A backend table of record-pair edges (one row per edge).
#' @param id_a,id_b Character scalars naming the two endpoint columns in
#'   `edges`.
#' @param score Optional character scalar naming a per-edge score column in
#'   `edges`. When supplied, within-entity `rank` is ordered by descending
#'   best score (the maximum over a vertex's incident edges) and that best
#'   score is returned as an extra `score` column. When `NULL`, ranking
#'   falls back to the `rep_by`/`id` rule.
#' @param vertices Optional. All vertex ids to include, so that ids absent
#'   from every edge come back as their own singleton entity (rank 1,
#'   `rep` = self). Either an atomic vector of ids, or a table with an `id`
#'   column (plus any `rep_by` column). When `NULL`, only ids appearing in
#'   `edges` are returned.
#' @param rep_by Optional character scalar naming a priority column (on the
#'   `vertices` table) used to pick the canonical representative: the member
#'   with the smallest `rep_by` wins, ties broken by smallest `id`.
#' @param block_by Optional character vector of columns in `edges` used to
#'   run connected components per block (DuckDB backend).
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return One row per resolved vertex:
#' \describe{
#'   \item{id}{The vertex id.}
#'   \item{entity}{Integer entity (connected-component) label.}
#'   \item{rep}{The canonical representative id of the entity.}
#'   \item{rank}{Rank within the entity; rank 1 is the representative.}
#'   \item{score}{Best incident-edge score per vertex (only when `score` is
#'     supplied).}
#' }
#'
#' @export
resolve_entities <- new_generic(
  "resolve_entities",
  c("edges", "id_a", "id_b")
)

#' Exact Token-Set Links (Score-1.0 Prefilter)
#'
#' @description
#' Expose the exact, score-1.0 case of a joinery match as a cheap,
#' hash-joinable prefilter that returns **both** the exact links **and** the
#' unmatched residual — so a workflow can run `exact -> fuzzy(residual)`
#' declaratively instead of re-implementing the prefilter outside the package.
#'
#' Two records are an exact link iff **every column's token set is equal**
#' within the same block (`containment = "off"`, the default). This is the
#' same object as a fuzzy score of exactly 1.0, seen from the set-equality
#' side, and it is empty-column robust: two records with identical names and
#' both-empty streets link, which the weighted scorer's `1 - weight(col)`
#' ceiling silently rejects.
#'
#' @details
#' **The fingerprint rides the strategy's own preparers.** Tokenization goes
#' through [prepare_search_data()] — never a parallel SQL/string fingerprint —
#' so the verb cannot diverge from the engine's actual `score == 1.0` (e.g.
#' `normalize_text`'s `De-ASCII` maps `Ü -> UE`, while DuckDB's `strip_accents`
#' maps `Ü -> U`; a hand-rolled fingerprint would silently collapse
#' `Müller`/`Muller` into a false link).
#'
#' **Containment (opt-in third tier).** On clean, additive-drift corpora the
#' score-1.0 mass is pure containment (`base` token set ⊆ `target`), not
#' set-equality. `containment = "forward"` links `base ⊆ target`;
#' `"bidirectional"` also links `target ⊆ base` (a growing listing needs the
#' reverse direction). Containment is data-shape-dependent and over-links on
#' noisy corpora, so it is **never** the default. `min_base_rarity` gates out
#' trivially-contained low-information base records (gate on the base record's
#' summed rarity mass).
#'
#' @param base A backend table (data.table / DuckDB tbl).
#' @param strategy A `Search_Strategy` defining the preparers, block, and
#'   rarity used to tokenize.
#' @param target A second table to link `base` against. `NULL` (default)
#'   selects the **self-join (dedup) form**.
#' @param base_id Character scalar naming the id column in `base`.
#' @param target_id Character scalar naming the id column in `target`
#'   (defaults to `base_id`).
#' @param containment One of `"off"` (set-equality, default), `"forward"`
#'   (`base ⊆ target`), or `"bidirectional"` (either containment direction).
#' @param min_base_rarity Numeric. Containment guard: drop links whose base
#'   record carries summed rarity mass below this floor. Default `0` (no gate).
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{links}{The score-1.0 pairs. Self form: `id_a | id_b` (+ block cols),
#'     ready to feed [resolve_entities()]. Cross form: `base_id | target_id`
#'     (+ block cols).}
#'   \item{residual}{The **unmatched** ids (the exact complement
#'     `base \\ matched`), for rehydration via [materialize_records()]. Self
#'     form: `$ids`. Cross form: `$base` and `$target`.}
#' }
#'
#' @seealso [resolve_entities()] (consumes the self-form links),
#'   [materialize_records()] (rehydrates the residual).
#'
#' @export
exact_token_links <- new_generic(
  "exact_token_links",
  c("base", "strategy")
)

#' Deduplicate a Table
#'
#' @description
#' Generic function that removes or merges duplicate records from a table
#' based on duplicate pairs identified by `detect_duplicates()`.
#'
#' @param base_table A data.frame / tibble / data.table (or db table in other backends).
#' @param duplicates A table of duplicate pairs generated by detect_duplicates
#' @param id Character scalar naming the ID column in `base_table`.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A deduplicated version of `base_table`.
#'
#' @export
deduplicate_table <- new_generic("deduplicate_table",
                                 c("base_table", "duplicates", "id"))

#' Search for Candidate Matches Between Tables
#'
#' @description
#' Generic function that finds candidate record matches between two tables
#' based on token-based similarity scoring defined in a `Search_Strategy`.
#'
#' @param base_table A data.frame / tibble / data.table (or db table in other backends).
#' @param target_table A data.frame / tibble / data.table (or db table in other backends) to search against.
#' @param base_id Character scalar naming the ID column in `base_table`.
#' @param target_id Character scalar naming the ID column in `target_table`.
#' @param strategy A `Search_Strategy` object defining matching criteria.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return Data with candidate matches
#'
#' @export
search_candidates <- new_generic("search_candidates",
                                 c("base_table", "target_table",
                                   "base_id", "target_id", "strategy"))

#' Compute Token Rarity for Record Linkage
#'
#' `compute_rarity()` assigns a rarity score to each token produced by
#' [`prepare_search_data()`], using the rarity method defined in a
#' `Search_Strategy`.
#'
#' Rarity quantifies how informative a token is when comparing records.
#' In **joinery**, rarity is always computed:
#'
#' - using **one global rarity metric** specified in the strategy,
#' - **per column**, because each field has its own token distribution,
#' - **within each block** (if the strategy specifies `block_by`).
#'
#' The input `tokens` must be the long-format token table returned by
#' `prepare_search_data()`, containing at minimum:
#'
#' - an ID column,
#' - a `column` field indicating the source variable,
#' - a `token` field,
#' - a `row_id` identifying the originating record,
#' - and any `block_by` variables required by the strategy.
#'
#' Backends (e.g., data.frame, data.table, DuckDB relations) may implement
#' their own methods for this generic, but all must return the same logical
#' structure: the original token table with an added numeric `rarity` column.
#'
#' @param tokens A token table created by [prepare_search_data()], in any
#'   backend-specific representation. Must contain at least `column`, `token`,
#'   and `row_id`, plus any `block_by` columns.
#' @param strategy A `Search_Strategy` defining the rarity method, blocking
#'   variables, and field structure.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return The same token table with an added `rarity` column.
#'
#' @export
compute_rarity <- new_generic(
  "compute_rarity",
  c("tokens", "strategy")
)


#' Extract Unmatched Records
#'
#' @description
#' Identify and extract records from a table that were
#' not matched in a record linkage operation.
#'
#' @param data A data.frame / tibble / data.table (or db table in other backends)
#'   containing the original records.
#' @param id Character scalar naming the ID column in `data`.
#' @param matches A table of matched record pairs, containing the ID column.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A subset of `data` containing only records whose IDs do not appear
#'   in `matches`.
#'
#' @export
extract_unmatched <- new_generic(
  "extract_unmatched",
  c("data", "id", "matches")
)


#' Materialize Records by ID
#'
#' @description
#' Rehydrate a set of record IDs back into their **full records**. The
#' positive (semi-join) complement of [extract_unmatched()]: where
#' `extract_unmatched()` *produces* a residual set of IDs, `materialize_records()`
#' pulls those IDs back into complete, scorable rows for the next stage.
#'
#' @details
#' `ids` is **polymorphic**. It may be either
#'
#' - an atomic vector of ID values, or
#' - a table (data.frame / data.table / backend tbl) carrying the IDs. The
#'   lookup order for the ID column is: a column literally named `id` first
#'   (the [extract_unmatched()] / `resolve_entities()` output convention),
#'   otherwise a column named the same as `id`.
#'
#' The return is a **semi-join**: IDs absent from `data` are silently dropped
#' (there is nothing to rehydrate), never NULL-filled. IDs are coerced to a
#' common type on both sides, so a BIGINT-corpus / character-id request still
#' matches. Row order is not guaranteed; the caller sorts if needed.
#'
#' On the DuckDB backend the IDs are **always** registered as a temp table and
#' joined — never inlined as an `id IN (<literal list>)`, which binds in
#' roughly O(n^2) and pins cores for minutes on large residual sets.
#'
#' @param data A data.frame / tibble / data.table (or db table in other
#'   backends) — the corpus to pull records from.
#' @param id Character scalar naming the ID column in `data`.
#' @param ids Either an atomic vector of ID values, or a table carrying them
#'   (read from an `id` column, else a column named `id`'s value).
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return The rows of `data` whose ID is in `ids`, all columns intact, one
#'   row per matching record, in no guaranteed order.
#'
#' @seealso [extract_unmatched()], the negative complement that produces the
#'   residual IDs this verb rehydrates.
#'
#' @export
materialize_records <- new_generic(
  "materialize_records",
  c("data", "id")
)


#' Multi-stage record linkage
#'
#' @description
#' Generic for running several search strategies in sequence on a pair of
#' tables. Backend methods are responsible for executing the per-stage
#' matching, removing matched rows, and combining results.
#'
#' @param base_table The left table in the linkage.
#' @param target_table The right table in the linkage.
#' @param base_id Character scalar naming the ID column in `base_table`.
#' @param target_id Character scalar naming the ID column in `target_table`.
#' @param strategies Named list of `Search_Strategy` objects defining the
#'   stages to run (in order).
#' @param ... Additional backend-specific arguments.
#'
#' @return A backend-specific match table, or `NULL` if no matches are found.
#'
#' @export
multi_stage_match <- new_generic(
  "multi_stage_match",
  c("base_table", "target_table", "base_id", "target_id", "strategies")
)



#' @noRd
.inspect_tokens <- new_generic(
  ".inspect_tokens",
  c("data", "id", "strategy", "column")
)

#' Inspect Tokens for a Specific Column
#'
#' @description
#' Extract and examine the tokens generated for a specific column
#' after applying the preprocessing steps defined in a `Search_Strategy`.
#' Useful for debugging and understanding how text is tokenized.
#'
#' @param data A data.frame / tibble / data.table (or db table in other backends).
#' @param id Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object defining preprocessing steps.
#' @param column <[`data-masked`][rlang::args_data_masking]> The column to inspect.
#'
#' @return A backend-specific table showing the tokens generated for the
#'   specified column.
#'
#' @export
inspect_tokens <- function(data, id, strategy, column) {
  column_chr <- rlang::as_name(rlang::ensym(column))
  .inspect_tokens(data, id, strategy, column_chr)
}
