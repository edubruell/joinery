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
#' @description
#' Turn a table into the long-format token table the matching verbs work on:
#' it applies each column's preparation steps, splits the text into tokens, and
#' attaches the id and any blocking columns. The other verbs
#' ([detect_duplicates()], [search_candidates()]) call this for you, so you
#' rarely need it directly; reach for it when you want to see or post-process
#' the tokens yourself.
#'
#' @param data A data.frame / tibble / data.table (or db table in other backends).
#' @param id   Character scalar naming the ID column in `data`.
#' @param strategy A `Search_Strategy` object.
#' @param ... Additional arguments passed to backend-specific methods.
#'
#' @return A long-format token table with one row per token, carrying the id,
#'   the source `column`, the `token`, a `row_id`, and any blocking columns.
#'
#' @seealso [inspect_tokens()] for a quick per-column look at the tokens.
#'
#' @export
prepare_search_data <- new_generic(
  "prepare_search_data",
  c("data", "id", "strategy")
)

#' Detect Duplicate Records
#'
#' @description
#' Find likely duplicate records inside a single table and group them.
#' Records are compared by how much of their rare, informative token content
#' they share (not by character-level edit distance), every pair is scored,
#' and any pair scoring at or above the threshold is linked. Records that link
#' directly or transitively form one duplicate group.
#'
#' Pass a [search_strategy()] for fuzzy, scored matching, or an
#' [exact_strategy()] to group only records whose token sets are identical.
#'
#' @param base_table A data.frame, tibble, data.table, or backend table to
#'   deduplicate.
#' @param id Character scalar naming the ID column in `base_table`.
#' @param strategy A `Search_Strategy` (or `Exact_Strategy`) describing how to
#'   tokenize each column, how to block, and the matching threshold.
#' @param ... Additional arguments passed to backend-specific methods. The
#'   most useful are `threshold` (override the strategy's threshold) and
#'   `weights` (a named numeric vector overriding the strategy's column
#'   weights).
#'
#' @return A table with one row per record that belongs to a duplicate group:
#' \describe{
#'   \item{duplicate_group}{Group label shared by all records that are
#'     duplicates of one another.}
#'   \item{id}{The record ID.}
#'   \item{score}{The record's match score within its group.}
#'   \item{rank}{Rank within the group; rank 1 is the representative kept by
#'     [deduplicate_table()].}
#'   \item{<original columns>}{Every other column from `base_table`.}
#' }
#'
#' @seealso [deduplicate_table()] to collapse the groups, [search_candidates()]
#'   for the cross-table version, [multi_stage_dedup()] for staged passes.
#'
#' @examples
#' data(base_example)
#'
#' strat <- search_strategy(
#'   Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
#'   Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
#'   Ort      ~ normalize_text(),
#'   block_by = "Kreis",
#'   threshold = 0.8
#' )
#'
#' dups <- detect_duplicates(base_example, id = "id_base", strategy = strat)
#' head(dups)
#'
#' @export
detect_duplicates <- new_generic(
  "detect_duplicates",
  c("base_table", "id", "strategy")
)

#' Group Matched Pairs into Entities
#'
#' @description
#' Take a list of matched record pairs (an edge list) and turn it into
#' entities: records that link directly or through a chain of links are
#' grouped together, each group gets an `entity` number, and one record in
#' each group is marked as its representative.
#'
#' This is the grouping step [detect_duplicates()] performs internally, exposed
#' on its own so you can resolve any pair list into entities, for example the
#' output of [search_candidates()] or a set of links you assembled yourself.
#'
#' @details
#' The result does not depend on the order of rows in `edges`: the same pairs
#' always produce the same `entity`, `rep`, and `rank`. Entity numbers are
#' assigned by the smallest member id in each group. The representative (the
#' rank-1 member) is chosen by highest best `score` when a score column is
#' given, then by smallest `rep_by` when given, then by smallest `id`.
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
#' @examples
#' # r1-r2 and r2-r3 chain into one entity; r4-r5 form another
#' edges <- data.table::data.table(
#'   a = c("r1", "r2", "r4"),
#'   b = c("r2", "r3", "r5")
#' )
#' resolve_entities(edges, id_a = "a", id_b = "b")
#'
#' @export
resolve_entities <- new_generic(
  "resolve_entities",
  c("edges", "id_a", "id_b"),
  function(edges, id_a, id_b, score = NULL, vertices = NULL,
           rep_by = NULL, block_by = NULL, ...) {
    S7_dispatch()
  }
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
#' Find candidate matches between two tables: for each record on one side, the
#' records on the other side that share enough rare, informative token content
#' to score at or above the threshold. This is the cross-table counterpart of
#' [detect_duplicates()].
#'
#' Pass a [search_strategy()] for fuzzy, scored matching, or an
#' [exact_strategy()] to keep only pairs whose token sets are identical.
#'
#' @param base_table A data.frame, tibble, data.table, or backend table.
#' @param target_table The table to search against.
#' @param base_id Character scalar naming the ID column in `base_table`.
#' @param target_id Character scalar naming the ID column in `target_table`.
#' @param strategy A `Search_Strategy` (or `Exact_Strategy`) describing how to
#'   tokenize each column, how to block, and the matching threshold.
#' @param ... Additional arguments passed to backend-specific methods, such as
#'   `threshold` and `weights`.
#'
#' @return A table with two rows per matched pair (one for the base record, one
#'   for the target record), sharing a `match_id`:
#' \describe{
#'   \item{match_id}{Identifier shared by the two rows of a matched pair.}
#'   \item{score}{The pair's match score.}
#'   \item{source}{`"base"` or `"target"`.}
#'   \item{id}{The record ID.}
#'   \item{<original columns>}{Every other column from the source table.}
#'   \item{rank}{Rank of this candidate among a record's matches.}
#' }
#'
#' @seealso [detect_duplicates()] for the within-table version,
#'   [extract_unmatched()] for the residual, [multi_stage_search()] for staged
#'   passes.
#'
#' @examples
#' data(base_example)
#' data(target_example)
#'
#' strat <- search_strategy(
#'   Nachname ~ normalize_text() + word_tokens(min_nchar = 3),
#'   Vorname  ~ normalize_text() + word_tokens(min_nchar = 3),
#'   Ort      ~ normalize_text(),
#'   block_by = "Kreis",
#'   threshold = 0.8
#' )
#'
#' matches <- search_candidates(
#'   base_example, target_example,
#'   base_id = "id_base", target_id = "id_target",
#'   strategy = strat
#' )
#' head(matches)
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
#' joined - never inlined as an `id IN (<literal list>)`, which binds in
#' roughly O(n^2) and pins cores for minutes on large residual sets.
#'
#' @param data A data.frame / tibble / data.table (or db table in other
#'   backends) - the corpus to pull records from.
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
  c("data", "id"),
  function(data, id, ids, ...) {
    S7_dispatch()
  }
)


#' Staged Search Across Tables or Sources
#'
#' @description
#' Link the same real-world entity across two tables, or across several
#' datasets or vintages of one dataset, by running an ordered list of
#' strategies as successive search passes. Each pass adds the links it finds to
#' a running record of every match (the `ledger`), and at the end all the links
#' are grouped into entities, one row per record showing which entity it
#' belongs to.
#'
#' A typical run starts with a cheap [exact_strategy()] pass to catch the clean
#' matches, then applies one or more looser [search_strategy()] passes to the
#' records still unmatched. Use this when the two sides are not interchangeable:
#' for example one record may carry only part of another's information, so it
#' matters which side is searched against which. For finding duplicates within
#' a single table, use [multi_stage_dedup()] instead.
#'
#' @param base_table The left table in the linkage.
#' @param target_table The right table. Pass `base_table` again with
#'   `self = TRUE` to search a single pooled table against itself.
#' @param base_id Character scalar naming the ID column in `base_table`.
#' @param target_id Character scalar naming the ID column in `target_table`.
#' @param strategies Named, ordered list of strategies to apply in turn. Each
#'   element is an [exact_strategy()], [search_strategy()], or
#'   [embedding_strategy()].
#' @param ... Further arguments controlling the staged run:
#'   * `self`: logical; `TRUE` searches `base_table` against itself (for
#'     example, pooling several years into one table and linking across them).
#'   * `source_by`: optional character vector naming the column(s) that record
#'     where each row came from (for example `"year"` or `"register"`). When
#'     set, every link is tagged as within-source or cross-source, and the
#'     result reports each entity's `source` and `covered_sources`.
#'   * `collapse`: what happens between stages. `"none"` only carries the
#'     still-unmatched records forward, while `"rep"` also collapses each group
#'     found so far to a single representative, shrinking the search space for
#'     the looser passes that follow.
#'   * `rep_rule`: rule for choosing each group's representative.
#'   * `rebind`: how the next stage's two sides are formed from the
#'     representatives and the residual: `"explicit"`, `"self"`, or
#'     `"accumulate"` (the path for incremental panel updates).
#'   * `direction`: which way each pass searches: `"forward"`, `"backward"`, or
#'     `"bidirectional"`.
#'   * `edge_filter`: optional callback `function(edges, stage_name)` applied to
#'     each pass's links before they are accumulated (for example a domain rule
#'     that drops implausible matches).
#'   * `rep_by`: optional priority column for choosing representatives (passed
#'     to [resolve_entities()]).
#'
#'   Backend methods may accept additional arguments.
#'
#' @return One row per pooled record describing its entity:
#'   `entity | id | rep | rank | score | source | covered_sources |
#'   n_in_entity | stage`. The full list of links found, with the stage and
#'   direction of each, is attached as the `ledger` attribute and read with
#'   `attr(result, "ledger")`.
#'
#' @seealso [multi_stage_dedup()] for the within-one-table version,
#'   [resolve_entities()] for the grouping step, [exact_strategy()] for the
#'   usual front stage.
#'
#' @export
multi_stage_search <- new_generic(
  "multi_stage_search",
  c("base_table", "target_table", "base_id", "target_id", "strategies")
)


#' Staged Duplicate Detection (within one table)
#'
#' @description
#' Deduplicate a single table in increasingly tolerant passes. A typical run
#' starts with a cheap [exact_strategy()] pass that catches the clean
#' duplicates, then applies looser [search_strategy()] passes (often with
#' wider blocking) to the records still unmatched. All the links found across
#' the passes are grouped into duplicate groups at the end, so a record linked
#' to `B` in an early pass and `B` linked to `C` in a later one all land in the
#' same group.
#'
#' For linking across two tables or several sources, use [multi_stage_search()].
#'
#' @param table A data.frame, tibble, data.table, or backend table to
#'   deduplicate.
#' @param id Character scalar naming the ID column in `table`.
#' @param strategies Named, ordered list of strategies to apply in turn. Each
#'   element is an [exact_strategy()], [search_strategy()], or
#'   [embedding_strategy()].
#' @param ... Further arguments to the staged run:
#'   * `rep_by`: optional character scalar naming a priority column on `table`
#'     used to choose each group's representative (passed to
#'     [resolve_entities()]: smallest `rep_by` wins, ties broken by smallest
#'     id).
#'   * `edge_filter`: optional callback `function(edges, stage_name)` applied to
#'     each pass's links before they are accumulated (for example a domain rule
#'     that drops implausible matches). The links carry `from`, `to`, `score`,
#'     and `stage`.
#'
#'   Backend methods may accept additional arguments.
#'
#' @return The standard dedup result: `duplicate_group | id | score | rank`
#'   plus the original columns of `table`, and a `stage` column recording which
#'   pass first linked each record.
#'
#' @seealso [multi_stage_search()] for the cross-table version,
#'   [detect_duplicates()] for a single pass, [resolve_entities()] for the
#'   grouping step.
#'
#' @export
multi_stage_dedup <- new_generic(
  "multi_stage_dedup",
  c("table", "id", "strategies")
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
