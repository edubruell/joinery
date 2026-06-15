# ============================================================
# data.table backend — exact (score-1.0) token-set matching
# ============================================================
#
# detect_duplicates() and search_candidates() methods for Exact_Strategy on
# in-memory data.table inputs, plus the fingerprint / containment kernel they
# share. Exactness reaches the standard verbs by dispatch (Exact_Strategy is a
# sibling of Search_Strategy / Embedding_Strategy); both methods return the
# standard joinery schema with score == 1.0.
#
# The fingerprint ALWAYS rides prepare_search_data() (via the proxy strategy)
# — never a parallel string fingerprint — so it cannot diverge from the
# engine's actual score == 1.0 (the normalize_text Ü->UE vs strip_accents Ü->U
# footgun). DT_tbl is defined in methods_datatable_prepare.R.
# ============================================================


# ---- fingerprint / containment kernel --------------------------------------

# One-row-per-record fingerprint table: id | <block_by> | fp_<col1> | ... .
# A column with an empty token set becomes "" (not NA) so empty<->empty is an
# equality (the §25 empty-column-robust dedup).
.exact_fp_wide_dt <- function(tokens, id_col, block_by, strategy_cols, delim) {
  tok <- .collapse_token_set(data.table::as.data.table(tokens), id_col, block_by)

  fp <- tok[
    , .(fp = paste0(sort(unique(token)), collapse = delim)),
    by = c(id_col, block_by, "src_column")
  ]

  lhs  <- paste(c(id_col, block_by), collapse = " + ")
  wide <- data.table::dcast(
    fp, stats::as.formula(paste(lhs, "~ src_column")), value.var = "fp"
  )

  for (col in strategy_cols) {
    if (!col %in% names(wide)) wide[[col]] <- NA_character_
    v <- wide[[col]]
    v[is.na(v)] <- ""
    wide[[col]] <- v
  }
  data.table::setnames(wide, strategy_cols, paste0("fp_", strategy_cols))
  wide[]
}

# Set-equality links (containment = "off").
#
# Self/dedup form emits a GROUP-BY *star* (rep -> each member), NOT the
# all-pairs self-join: set-equality is transitive, so every record sharing a
# fingerprint is one entity and the N^2 clique pairs are pure waste. A block
# with K identical records (e.g. a directory-publisher row duplicated thousands
# of times) collapses in O(K) star edges instead of O(K^2) pairs. resolve_entities
# yields identical connected components. Mirrors v1 31_collapse_within_year.R.
#
# Empty-fingerprint guard: a record with no tokens in ANY column ('' in every
# fp_col) carries no identity and must not collapse — else every token-less row
# in a block merges into one spurious entity (and self-joins into an N^2 clique).
# Such rows stay singletons. (Distinct from the §25 empty-*column* case — an
# identical name with empty street still links; only ALL-empty is excluded.)
.exact_links_off_dt <- function(base, proxy, base_id,
                                target, target_id, block_by, self) {
  cols    <- names(proxy@preparers)
  fp_cols <- paste0("fp_", cols)
  keys    <- c(block_by, fp_cols)
  not_all_empty <- function(w) Reduce(`|`, lapply(fp_cols, function(cc) w[[cc]] != ""))

  wb <- .exact_fp_wide_dt(
    prepare_search_data(base, base_id, proxy),
    base_id, block_by, cols, .JOINERY_FP_DELIM
  )

  if (self) {
    wb <- wb[not_all_empty(wb)]
    if (nrow(wb) == 0L) {
      out <- data.table::data.table(id_a = character(), id_b = character())
      for (b in block_by) out[[b]] <- character()
      return(out[])
    }
    # rep = lexicographically smallest id per exact-fingerprint group; star edges
    # rep -> member (O(K)), never the K(K-1)/2 clique.
    data.table::setnames(wb, base_id, "._id")
    wb[, ._rep := min(._id), by = keys]
    links <- wb[._id != ._rep, c("._rep", "._id", block_by), with = FALSE]
    data.table::setnames(links, c("._rep", "._id"), c("id_a", "id_b"))
    links <- unique(links)
  } else {
    wt <- .exact_fp_wide_dt(
      prepare_search_data(target, target_id, proxy),
      target_id, block_by, cols, .JOINERY_FP_DELIM
    )
    wb <- wb[not_all_empty(wb)]
    wt <- wt[not_all_empty(wt)]
    data.table::setnames(wb, base_id,   "base_id")
    data.table::setnames(wt, target_id, "target_id")
    links <- merge(wb, wt, by = keys, allow.cartesian = TRUE)
    links <- unique(links[, c("base_id", "target_id", block_by), with = FALSE])
  }
  links[]
}

# Containment links (forward / bidirectional). Structural subset test —
# overlap-join on shared tokens, |base ∩ target| == |base| (forward) and/or
# == |target| (reverse), gated on base rarity mass. Returns ._bid | ._tid |
# <block_by>.
.exact_links_containment_dt <- function(base, proxy, base_id,
                                        target, target_id, block_by, self,
                                        mode, min_base_rarity,
                                        min_containment_tokens = 1) {
  btok <- compute_rarity(prepare_search_data(base, base_id, proxy), proxy)
  btok <- .collapse_token_set(data.table::as.data.table(btok), base_id, block_by)

  if (self) {
    ttok <- data.table::copy(btok)
    tid  <- base_id
  } else {
    ttok <- prepare_search_data(target, target_id, proxy)
    ttok <- .collapse_token_set(data.table::as.data.table(ttok), target_id, block_by)
    tid  <- target_id
  }

  b <- btok[, c(base_id, block_by, "src_column", "token", "rarity"), with = FALSE]
  data.table::setnames(b, base_id, "._bid")
  t <- ttok[, c(tid, block_by, "src_column", "token"), with = FALSE]
  data.table::setnames(t, tid, "._tid")

  nbase   <- b[, .(n_base = .N, rmass = sum(rarity)), by = "._bid"]
  ntarget <- t[, .(n_target = .N), by = "._tid"]
  bblock  <- unique(b[, c("._bid", block_by), with = FALSE])

  key <- c(block_by, "src_column", "token")
  ov  <- merge(
    b[, c("._bid", key), with = FALSE],
    t[, c("._tid", key), with = FALSE],
    by = key, allow.cartesian = TRUE
  )
  if (nrow(ov) == 0L) {
    return(data.table::data.table(._bid = character(), ._tid = character()))
  }

  nmatch <- ov[, .(n_match = .N), by = c("._bid", "._tid")]
  nmatch <- merge(nmatch, nbase,   by = "._bid")
  nmatch <- merge(nmatch, ntarget, by = "._tid")

  # Containment qualifiers. The direction test is pooled (n_match == n_base for
  # forward = base subset of target), but the min_containment_tokens guard is
  # applied PER COLUMN: a link is dropped if any column in which the contained
  # side is a NON-EMPTY proper subset carries fewer than mc tokens. Per-column is
  # essential for multi-column strategies - otherwise street tokens shared across a
  # building's tenants pad a generic NAME past the threshold (the YP shopping-mall
  # / Aerztehaus chain). Empty contained columns are ignored, preserving the §25
  # empty-column robustness; mc == 1 gates nothing (prior behaviour exactly).
  mc <- min_containment_tokens
  if (mc > 1) {                              # per-column counts only feed the gate
    bc <- b[, .(n_base_c   = .N), by = c("._bid", "src_column")]
    tc <- t[, .(n_target_c = .N), by = c("._tid", "src_column")]
  }

  gate_fwd <- function(pairs) {              # base is the contained side
    if (mc <= 1 || nrow(pairs) == 0L) return(pairs)
    cmp <- merge(pairs, bc, by = "._bid", allow.cartesian = TRUE)
    cmp <- merge(cmp, tc, by = c("._tid", "src_column"))
    bad <- unique(cmp[n_base_c < n_target_c & n_base_c < mc, .(._bid, ._tid)])
    if (nrow(bad)) pairs[!bad, on = c("._bid", "._tid")] else pairs
  }
  gate_rev <- function(pairs) {              # target is the contained side
    if (mc <= 1 || nrow(pairs) == 0L) return(pairs)
    cmp <- merge(pairs, tc, by = "._tid", allow.cartesian = TRUE)
    cmp <- merge(cmp, bc, by = c("._bid", "src_column"))
    bad <- unique(cmp[n_target_c < n_base_c & n_target_c < mc, .(._bid, ._tid)])
    if (nrow(bad)) pairs[!bad, on = c("._bid", "._tid")] else pairs
  }

  fwd <- gate_fwd(nmatch[n_match == n_base, .(._bid, ._tid)])
  qualifying <- if (mode == "bidirectional") {
    rev <- gate_rev(nmatch[n_match == n_target, .(._bid, ._tid)])
    data.table::rbindlist(list(fwd, rev))
  } else {
    fwd
  }

  qualifying <- unique(qualifying)
  qualifying <- merge(qualifying, nbase[, .(._bid, rmass)], by = "._bid", all.x = TRUE)
  qualifying <- qualifying[rmass >= min_base_rarity]
  if (self) qualifying <- qualifying[._bid != ._tid]
  qualifying <- unique(qualifying[, .(._bid, ._tid)])
  qualifying <- merge(qualifying, bblock, by = "._bid", all.x = TRUE)
  qualifying[]
}

# Self-form links: id_a | id_b | <block_by>, handling off + containment.
.exact_self_links_dt <- function(base, proxy, id, block_by, containment, min_base_rarity,
                                 min_containment_tokens = 1) {
  if (containment == "off") {
    return(.exact_links_off_dt(base, proxy, id, NULL, id, block_by, self = TRUE))
  }
  raw <- .exact_links_containment_dt(base, proxy, id, NULL, id, block_by,
                                     self = TRUE, containment, min_base_rarity,
                                     min_containment_tokens)
  if (nrow(raw) == 0L) {
    return(data.table::data.table(id_a = character(), id_b = character()))
  }
  links <- raw[, c("._bid", "._tid", block_by), with = FALSE]
  data.table::setnames(links, c("._bid", "._tid"), c("id_a", "id_b"))
  # normalize undirected orientation, drop duplicates
  links[, `:=`(lo = pmin(id_a, id_b), hi = pmax(id_a, id_b))]
  links[, `:=`(id_a = lo, id_b = hi)][, c("lo", "hi") := NULL]
  unique(links[id_a < id_b, c("id_a", "id_b", block_by), with = FALSE])
}

# Cross-form links: base_id | target_id | <block_by>, handling off + containment.
.exact_cross_links_dt <- function(base, proxy, base_id, target, target_id,
                                  block_by, containment, min_base_rarity,
                                  min_containment_tokens = 1) {
  if (containment == "off") {
    return(.exact_links_off_dt(base, proxy, base_id, target, target_id, block_by, self = FALSE))
  }
  raw <- .exact_links_containment_dt(base, proxy, base_id, target, target_id,
                                     block_by, self = FALSE, containment, min_base_rarity,
                                     min_containment_tokens)
  if (nrow(raw) == 0L) {
    return(data.table::data.table(base_id = character(), target_id = character()))
  }
  links <- raw[, c("._bid", "._tid", block_by), with = FALSE]
  data.table::setnames(links, c("._bid", "._tid"), c("base_id", "target_id"))
  links[]
}


# ---- detect_duplicates (self / dedup face) ---------------------------------

method(
  detect_duplicates,
  list(DT_tbl, class_character, Exact_Strategy)
) <- function(base_table, id, strategy, ...) {

  dt <- data.table::copy(base_table)
  dt[[id]] <- as.character(dt[[id]])
  block_by <- strategy@block_by %||% character()
  proxy    <- .exact_proxy_strategy(strategy)

  links <- .exact_self_links_dt(dt, proxy, id, block_by,
                                strategy@containment, strategy@min_base_rarity,
                                strategy@min_containment_tokens)

  empty <- function() data.table::data.table(
    duplicate_group = integer(), id = character(),
    score = numeric(), rank = integer()
  )
  if (nrow(links) == 0L) return(empty())

  # Connected components over the exact links; exact => score 1.0.
  ent <- resolve_entities(
    links[, .(from = id_a, to = id_b)], "from", "to",
    vertices = unique(dt[[id]])
  )
  ent[, n := .N, by = entity]
  dups <- ent[n > 1L]
  if (nrow(dups) == 0L) return(empty())

  dups[, score := 1.0]
  data.table::setnames(dups, "entity", "duplicate_group")
  result <- dups[, .(id, duplicate_group, score, rank)]
  data.table::setkeyv(result, c("duplicate_group", "rank"))

  result <- merge(result, dt, by.x = "id", by.y = id, all.x = TRUE, sort = FALSE)
  result[]
}


# ---- search_candidates (cross / search face) -------------------------------

method(
  search_candidates,
  list(DT_tbl, DT_tbl, class_character, class_character, Exact_Strategy)
) <- function(base_table, target_table, base_id, target_id, strategy, ...) {

  base_dt   <- data.table::copy(base_table)
  target_dt <- data.table::copy(target_table)
  base_dt[[base_id]]     <- as.character(base_dt[[base_id]])
  target_dt[[target_id]] <- as.character(target_dt[[target_id]])

  block_by <- strategy@block_by %||% character()
  proxy    <- .exact_proxy_strategy(strategy)

  links <- .exact_cross_links_dt(base_dt, proxy, base_id, target_dt, target_id,
                                 block_by, strategy@containment, strategy@min_base_rarity,
                                 strategy@min_containment_tokens)

  if (nrow(links) == 0L) {
    return(data.table::data.table(
      match_id = integer(), score = numeric(),
      source = character(), id = character(), rank = integer()
    ))
  }

  links[, match_id := .I]
  long <- data.table::rbindlist(list(
    links[, .(match_id, score = 1.0, source = "base",   id = base_id)],
    links[, .(match_id, score = 1.0, source = "target", id = target_id)]
  ))

  base_dt2   <- data.table::copy(base_dt);   base_dt2[, id := base_dt2[[base_id]]]
  target_dt2 <- data.table::copy(target_dt); target_dt2[, id := target_dt2[[target_id]]]

  base_long <- merge(long[source == "base"], base_dt2,
                     by = "id", all.x = TRUE, sort = FALSE)
  target_long <- merge(long[source == "target"], target_dt2,
                       by = "id", all.x = TRUE, sort = FALSE)

  out <- data.table::rbindlist(list(base_long, target_long), use.names = TRUE, fill = TRUE)
  out[, rank := rank(-score, ties.method = "first"), by = match_id]
  data.table::setorder(out, match_id, source, rank)
  out[]
}
