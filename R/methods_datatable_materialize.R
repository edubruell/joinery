# ============================================================
# data.table backend — materialize records by ID
# ============================================================
#
# `materialize_records()` for in-memory data.table inputs. The positive
# (semi-join) complement of `extract_unmatched()` in
# `methods_datatable_multistage.R`.
#
# ============================================================


# Resolve the `ids` argument (vector OR table) into an atomic vector of IDs.
# Lookup order for a table: an "id" column first, then a column named the
# same as `data`'s id column.
.materialize_id_values <- function(ids, id) {
  if (is.data.frame(ids)) {
    col <- if ("id" %in% names(ids)) {
      "id"
    } else if (id %in% names(ids)) {
      id
    } else {
      cli::cli_abort(
        "{.arg ids} table must contain a column named {.field id} or {.field {id}}"
      )
    }
    return(ids[[col]])
  }
  ids
}


# Method: materialize_records
#------------------------------------------------------------------------------
method(
  materialize_records,
  list(DT_tbl, class_character)
) <- function(data, id, ids, ...) {
  dt <- data.table::copy(data)

  if (!id %in% names(dt)) {
    cli::cli_abort("ID column {.field {id}} not found in data")
  }

  id_vals <- .materialize_id_values(ids, id)

  # Normalize types on both sides (BIGINT-corpus / character-id parity),
  # mirroring extract_unmatched()'s coercion.
  dt[[id]] <- as.character(dt[[id]])
  ids_chr  <- unique(as.character(id_vals))

  # Keyed semi-join: scales to >10k ids without an O(n) scan per id.
  data.table::setkeyv(dt, id)
  out <- dt[list(ids_chr), nomatch = NULL]
  out[]
}
