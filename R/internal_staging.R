# ============================================================
# Shared staged-linkage engine
# ============================================================
#
# The between-stage machinery shared by the two staged verbs:
#
#   multi_stage_dedup   (R/methods_*_multistage.R) — dedup face
#   multi_stage_search  (R/methods_*_multistage.R) — search face
#
# Both are thin configs over the loop here. The engine never invents a
# connected-components or residual-rehydrate primitive of its own: it composes
# the shipped generics
#
#   detect_duplicates() / search_candidates()  — per-stage apply (S7 dispatch
#                                                 picks Exact_/Search_/Embedding_)
#   extract_unmatched() / materialize_records() — residual carry-forward
#   resolve_entities()                          — the ONLY connected components
#
# The load-bearing invariant (both faces): accumulate an edge set over the
# *original* ids, then resolve to entities with resolve_entities(). Collapse is
# a performance optimisation, never a semantic shortcut — a collapsing stage
# records the `member ~ rep` edges so a final resolve over the original
# vertices still closes every member into the right entity.
#
# Strategies are homogeneous over strategy *kind*: each element is an
# Exact_Strategy, Search_Strategy, or Embedding_Strategy, and the loop calls one
# apply verb and lets S7 pick the method — there is no "is this exact?" branch.
# ============================================================


# ---------------------------------------------------------------------------
# Strategy-list validation + naming (shared by every staged method, both
# backends). Admits Exact_Strategy alongside Search_/Embedding_Strategy — the
# gate that lets an exact front stage reach its method by dispatch.
# ---------------------------------------------------------------------------

#' @noRd
.is_stage_strategy <- function(s) {
  S7_inherits(s, Search_Strategy) ||
    S7_inherits(s, Exact_Strategy) ||
    S7_inherits(s, Embedding_Strategy)
}

#' Validate the search-face policy axes; abort on unimplemented combinations.
#' @noRd
.check_search_policy <- function(collapse, rebind, direction, rep_rule) {
  collapse  <- match.arg(collapse,  c("none", "rep", "union"))
  rebind    <- match.arg(rebind,    c("explicit", "self", "accumulate"))
  direction <- match.arg(direction, c("forward", "backward", "bidirectional"))
  rep_rule  <- match.arg(rep_rule,
                         c("canonical", "newest", "longest_lived",
                           "most_complete", "union"))
  if (collapse == "union") {
    cli::cli_abort(c(
      "{.code collapse = \"union\"} (member token-set merge) is not implemented yet.",
      "i" = "Use {.code collapse = \"rep\"} (one representative per group) or {.code \"none\"}."
    ))
  }
  if (rep_rule != "canonical") {
    cli::cli_abort(c(
      "{.code rep_rule = \"{rep_rule}\"} is not implemented yet.",
      "i" = "Only {.val canonical} is wired; pass {.arg rep_by} for an explicit representative-priority column."
    ))
  }
  list(collapse = collapse, rebind = rebind, direction = direction,
       rep_rule = rep_rule)
}

#' Validate + name an ordered strategy list for a staged verb.
#'
#' Returns the list with stage names filled in (`strategy_1`, … when missing).
#' Aborts on an empty list or any element that is not a recognised strategy
#' kind.
#' @noRd
.stage_strategies <- function(strategies) {
  if (!is.list(strategies) || length(strategies) == 0L) {
    cli::cli_abort("{.arg strategies} must be a non-empty list.")
  }
  if (is.null(names(strategies)) || any(names(strategies) == "")) {
    names(strategies) <- paste0("strategy_", seq_along(strategies))
  }
  if (!all(map_lgl(strategies, .is_stage_strategy))) {
    cli::cli_abort(
      "{.arg strategies} must be a list of {.cls Exact_Strategy}, \\
       {.cls Search_Strategy}, or {.cls Embedding_Strategy} objects."
    )
  }
  strategies
}


# ---------------------------------------------------------------------------
# Backend-agnostic collect: bring a per-stage apply result (small relative to
# the corpus — only the matched rows) into a data.table for edge accumulation.
# ---------------------------------------------------------------------------

#' @noRd
.collect_stage_tbl <- function(x) {
  if (data.table::is.data.table(x)) return(x)
  if (is.data.frame(x))             return(data.table::as.data.table(x))
  data.table::as.data.table(dplyr::collect(x))   # backend tbl (DuckDB, …)
}


# ---------------------------------------------------------------------------
# groups -> edges (the one composition decision, 06).
#
# detect_duplicates() returns formatted *groups* (it runs resolve_entities
# internally per stage). The staged dedup verb needs raw *edges* to accumulate
# for the final connected-components, so it expands each group into a STAR of
# edges (rep <-> every other member). A star is lossless for connectivity and
# keeps the verb composing the public apply verb rather than a private links
# kernel.
# ---------------------------------------------------------------------------

#' Expand a detect_duplicates groups table into a star edge list.
#'
#' Returns `data.table(from, to, score)` plus, as an attribute `"non_reps"`, the
#' ids of the non-representative members — the rows the staged loop drops from
#' the working set (keeping the rep, an original id, so a later looser stage can
#' still bridge a drifted record into the cluster).
#' @noRd
.star_expand_groups <- function(groups) {
  g <- .collect_stage_tbl(groups)
  if (nrow(g) == 0L) {
    out <- data.table::data.table(
      from = character(), to = character(), score = numeric()
    )
    data.table::setattr(out, "non_reps", character(0))
    return(out)
  }
  g <- g[, .(duplicate_group, id = as.character(id), score, rank)]
  reps <- g[rank == 1L, .(duplicate_group, rep = id)]
  # rep -> every non-rep member of the same group; the member carries the score.
  members <- g[rank != 1L]
  e <- members[reps, on = "duplicate_group", nomatch = 0L]
  e <- e[, .(from = rep, to = id, score = score)]
  data.table::setattr(e, "non_reps", unique(members$id))
  e
}


# ---------------------------------------------------------------------------
# The staged DEDUP engine (collapse = residual, rebind = self, direction =
# bidirectional). Runs detect_duplicates per stage, star-expands into edges,
# accumulates over original ids, and resolves connected components ONCE at the
# end.
#
# Working-set carry-forward: after a stage, drop only the *non-representative*
# members of each found group; the representative (a real original id) and every
# still-unmatched singleton stay. This (a) shrinks the working set each looser
# stage scans, and (b) keeps a bridge in place so a record that drifts away from
# a cluster's other members can still attach to that cluster's representative at
# a later, looser stage — which a pure drop-all-matched residual would break
# (the §29 cross-stage transitive-closure case). Because the carried rep is an
# original id, every accumulated edge is over original ids and the single final
# resolve_entities() closes each member into the right entity.
#
# Returns a list:
#   edges     data.table(from, to, score, stage)  — accumulated, over orig ids
#   all_ids   character()                          — every id in `table`
# The caller (the per-backend method) runs resolve_entities() on `edges` and
# formats the standard dedup schema. There is no CC and no residual rehydrate
# in here other than the composed generics.
# ---------------------------------------------------------------------------

#' @noRd
.run_staged_dedup <- function(table, id, strategies, all_ids,
                              edge_filter = NULL) {
  strategies <- .stage_strategies(strategies)

  working     <- table
  kept        <- all_ids               # ids still carried into the next stage
  edge_chunks <- list()

  for (stage_name in names(strategies)) {
    groups <- detect_duplicates(working, id, strategies[[stage_name]])
    e <- .star_expand_groups(groups)
    non_reps <- attr(e, "non_reps", exact = TRUE)

    if (nrow(e) > 0L) {
      e[, stage := stage_name]
      if (!is.null(edge_filter)) {
        e <- data.table::as.data.table(edge_filter(e, stage_name))
      }
      if (nrow(e) > 0L) edge_chunks[[stage_name]] <- e
    }

    # Drop the non-representative members; keep reps (bridges) + singletons.
    new_kept <- setdiff(kept, non_reps)
    if (length(new_kept) == length(kept)) next   # nothing absorbed this stage
    kept <- new_kept
    if (length(kept) == 0L) break

    # Rehydrate the surviving working set for the next, looser stage.
    working <- materialize_records(table, id, kept)
  }

  edges <- if (length(edge_chunks) == 0L) {
    data.table::data.table(
      from = character(), to = character(),
      score = numeric(), stage = character()
    )
  } else {
    data.table::rbindlist(edge_chunks, use.names = TRUE)
  }

  list(edges = edges, all_ids = all_ids)
}


# ---------------------------------------------------------------------------
# Source-provenance helper. `source_by` is a generic provenance column (or
# composite) on the records; combine it into a single character key so the same
# code tags edges and counts covered sources whether the axis is "year",
# "register", or c("register","year").
# ---------------------------------------------------------------------------

#' @noRd
.combine_source <- function(dt, cols) {
  if (length(cols) == 1L) return(as.character(dt[[cols]]))
  do.call(paste, c(lapply(cols, function(cc) as.character(dt[[cc]])),
                   list(sep = .JOINERY_FP_DELIM)))
}


# ---------------------------------------------------------------------------
# pairs -> directed edges. search_candidates() returns the candidate schema
# (match_id | score | source | id | rank | <original columns>); within a
# match_id the source == "base" row is the searched record and the
# source == "target" rows are the found candidates. A directed edge runs
# base -> target, carrying the candidate's score and (when source_by is set)
# the source-pair tags.
# ---------------------------------------------------------------------------

#' @noRd
.pairs_to_edges <- function(pairs, stage_name, direction, source_by) {
  p <- .collect_stage_tbl(pairs)
  if (nrow(p) == 0L) {
    return(data.table::data.table(
      from = character(), to = character(), score = numeric(),
      stage = character(), source_from = character(),
      source_to = character(), within_source = logical(),
      direction = character()
    ))
  }
  p[, id := as.character(id)]

  if (is.null(source_by)) {
    b <- p[source == "base",   .(match_id, from = id)]
    t <- p[source == "target", .(match_id, to = id, score)]
  } else {
    bp <- p[source == "base"]
    tp <- p[source == "target"]
    b <- data.table::data.table(match_id = bp$match_id, from = bp$id,
                                source_from = .combine_source(bp, source_by))
    t <- data.table::data.table(match_id = tp$match_id, to = tp$id,
                                score = tp$score,
                                source_to = .combine_source(tp, source_by))
  }

  e <- t[b, on = "match_id", nomatch = 0L, allow.cartesian = TRUE]
  e <- e[from != to]                                   # drop self-pairs (self mode)
  if (nrow(e) == 0L) {
    return(data.table::data.table(
      from = character(), to = character(), score = numeric(),
      stage = character(), source_from = character(),
      source_to = character(), within_source = logical(),
      direction = character()
    ))
  }
  e[, stage := stage_name]
  e[, direction := direction]
  if (is.null(source_by)) {
    e[, `:=`(source_from = NA_character_, source_to = NA_character_,
             within_source = NA)]
  } else {
    e[, within_source := source_from == source_to]
  }
  e[, .(from, to, score, stage, source_from, source_to, within_source, direction)]
}


# ---------------------------------------------------------------------------
# The staged SEARCH engine (the search face). Directed cross-source linkage:
# per stage run search_candidates() (one orientation, or both for
# `bidirectional`), turn the pairs into directed edges, tag the source-pair,
# accumulate the ledger over original ids, and carry the working set forward
# under the chosen collapse / rebind policy. The single resolve_entities() into
# the cross-source entity grouping happens in the caller (it owns the vertex /
# source maps); per-stage resolve_entities() here is only to pick the
# representatives a `rep`/`accumulate` collapse keeps.
#
# Returns list(ledger = data.table(directed tagged edges over original ids)).
# ---------------------------------------------------------------------------

#' @noRd
.run_staged_search <- function(base, target, base_id, target_id, strategies,
                               self = FALSE, source_by = NULL,
                               collapse = "none", rep_rule = "canonical",
                               rebind = "explicit", direction = "forward",
                               edge_filter = NULL, rep_by = NULL) {
  strategies <- .stage_strategies(strategies)

  # One stage, possibly two orientations for `bidirectional`.
  run_one <- function(b_tbl, b_id, t_tbl, t_id, strategy, stage_name) {
    orient <- function(bt, bi, tt, ti, dir) {
      pairs <- search_candidates(bt, tt, bi, ti, strategy = strategy)
      .pairs_to_edges(pairs, stage_name, dir, source_by)
    }
    if (direction == "bidirectional") {
      data.table::rbindlist(list(
        orient(b_tbl, b_id, t_tbl, t_id, "forward"),
        orient(t_tbl, t_id, b_tbl, b_id, "backward")
      ), use.names = TRUE)
    } else if (direction == "backward") {
      orient(t_tbl, t_id, b_tbl, b_id, "backward")
    } else {
      orient(b_tbl, b_id, t_tbl, t_id, "forward")
    }
  }

  base_res   <- base
  target_res <- if (self) base else target
  bid <- base_id
  tid <- if (self) base_id else target_id

  ledger_chunks <- list()

  for (stage_name in names(strategies)) {
    e <- run_one(base_res, bid, target_res, tid, strategies[[stage_name]], stage_name)
    if (nrow(e) > 0L && !is.null(edge_filter)) {
      e <- data.table::as.data.table(edge_filter(e, stage_name))
    }
    if (nrow(e) > 0L) ledger_chunks[[stage_name]] <- e

    matched_from <- unique(e$from)
    matched_to   <- unique(e$to)
    if (length(matched_from) == 0L && length(matched_to) == 0L) next

    # Working-set carry-forward via materialize_records (the sanctioned
    # rehydrate; backend-uniform — extract_unmatched needs a backend-matching
    # matches table, which an R-side id vector is not on DuckDB).
    pooled <- self || rebind == "self"

    if (collapse == "none") {
      if (pooled) {
        keep <- setdiff(.ids_of(base_res, bid), union(matched_from, matched_to))
        base_res <- materialize_records(base, bid, keep); target_res <- base_res
      } else {
        base_res   <- materialize_records(
          base, bid, setdiff(.ids_of(base_res, bid), matched_from))
        target_res <- materialize_records(
          target, tid, setdiff(.ids_of(target_res, tid), matched_to))
        keep <- union(.ids_of(base_res, bid), .ids_of(target_res, tid))
      }
    } else {
      # Collapse-and-continue: resolve the ledger so far, keep one rep per
      # component (a real original id) plus every still-unmatched record, and
      # rebuild the working set(s) from those. The carried rep is the bridge.
      reps <- .stage_reps(data.table::rbindlist(ledger_chunks, use.names = TRUE),
                          rep_by = rep_by, rep_rule = rep_rule)$keep
      if (pooled) {
        keep <- union(reps, setdiff(.ids_of(base_res, bid),
                                    union(matched_from, matched_to)))
        base_res <- materialize_records(base, bid, keep); target_res <- base_res
      } else if (rebind == "accumulate") {
        # base grows (reps + base residual); target consumes its residual.
        base_res   <- materialize_records(
          base, bid, union(reps, .ids_of(base_res, bid)))
        target_res <- materialize_records(
          target, tid, setdiff(.ids_of(target_res, tid), matched_to))
        keep <- union(.ids_of(base_res, bid), .ids_of(target_res, tid))
      } else {
        # explicit: collapse each side to its reps + residual independently.
        b_ids <- .ids_of(base_res, bid)
        base_res   <- materialize_records(
          base, bid, union(intersect(reps, b_ids), setdiff(b_ids, matched_from)))
        target_res <- materialize_records(
          target, tid, setdiff(.ids_of(target_res, tid), matched_to))
        keep <- union(.ids_of(base_res, bid), .ids_of(target_res, tid))
      }
    }
    if (length(keep) == 0L) break
  }

  ledger <- if (length(ledger_chunks) == 0L) {
    data.table::data.table(
      from = character(), to = character(), score = numeric(),
      stage = character(), source_from = character(),
      source_to = character(), within_source = logical(),
      direction = character()
    )
  } else {
    data.table::rbindlist(ledger_chunks, use.names = TRUE)
  }
  list(ledger = ledger)
}


#' Collected ids of a working set (backend-agnostic).
#' @noRd
.ids_of <- function(tbl, id) {
  if (data.table::is.data.table(tbl) || is.data.frame(tbl)) {
    return(unique(as.character(tbl[[id]])))
  }
  unique(as.character(dplyr::pull(dplyr::distinct(tbl, !!rlang::sym(id)), 1)))
}


#' Pick one representative per current component of an accumulated ledger.
#' Returns list(keep = reps + still-unmatched ids carried forward).
#' rep selection is canonical (smallest id) unless `rep_by` is supplied.
#' @noRd
.stage_reps <- function(ledger, rep_by_tbl = NULL, rep_by = NULL,
                        rep_rule = "canonical") {
  if (nrow(ledger) == 0L) return(list(keep = character(0), keep_all = character(0)))
  ent <- resolve_entities(
    edges = ledger[, .(from, to, score)],
    id_a = "from", id_b = "to", score = "score"
  )
  reps <- unique(ent[rank == 1L]$rep)
  list(keep = reps, keep_all = reps)
}


# ---------------------------------------------------------------------------
# Final cross-source entity grouping (the search deliverable). Resolves the
# accumulated ledger ONCE over all pooled vertices (so unmatched records come
# back as singleton trajectories), attaches each record's source and its
# entity's covered-source count, the first stage that linked it, and carries
# the directed ledger as the `"ledger"` attribute.
#
# `vertices` is a data.table with at least `id`, plus the `source_by` column(s)
# and any `rep_by` column, one row per pooled record (unique by id).
# ---------------------------------------------------------------------------

#' @noRd
.finalize_search_grouping <- function(ledger, vertices, source_by, rep_by) {
  v <- data.table::as.data.table(vertices)
  v[, id := as.character(id)]
  v <- unique(v, by = "id")
  v[, source := if (is.null(source_by)) NA_character_ else .combine_source(v, source_by)]

  resolve_vertices <- if (is.null(rep_by)) v$id else v[, c("id", rep_by), with = FALSE]

  ent <- resolve_entities(
    edges    = ledger[, .(from, to, score)],
    id_a     = "from", id_b = "to", score = "score",
    vertices = resolve_vertices, rep_by = rep_by
  )
  ent <- v[, .(id, source)][ent, on = "id"]

  if (is.null(source_by)) {
    ent[, covered_sources := NA_integer_]
  } else {
    ent[, covered_sources := data.table::uniqueN(source), by = "entity"]
  }
  ent[, n_in_entity := .N, by = "entity"]

  if (nrow(ledger) > 0L) {
    stage_levels <- unique(ledger$stage)
    long <- data.table::rbindlist(list(
      ledger[, .(id = from, stage)], ledger[, .(id = to, stage)]
    ))
    long[, so := match(stage, stage_levels)]
    fs <- long[, .(stage = stage_levels[min(so)]), by = "id"]
    ent <- fs[ent, on = "id"]
  } else {
    ent[, stage := NA_character_]
  }

  data.table::setcolorder(
    ent,
    c("entity", "id", "rep", "rank", "score", "source",
      "covered_sources", "n_in_entity", "stage")
  )
  data.table::setorderv(ent, c("entity", "rank"))
  data.table::setattr(ent, "ledger", ledger[])
  ent[]
}
