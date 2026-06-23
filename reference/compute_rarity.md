# Compute Token Rarity for Record Linkage

`compute_rarity()` assigns a rarity score to each token produced by
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md),
using the rarity method defined in a `Search_Strategy`.

## Usage

``` r
compute_rarity(tokens, strategy, ...)
```

## Arguments

- tokens:

  A token table created by
  [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md),
  in any backend-specific representation. Must contain at least
  `column`, `token`, and `row_id`, plus any `block_by` columns.

- strategy:

  A `Search_Strategy` defining the rarity method, blocking variables,
  and field structure.

- ...:

  Additional arguments passed to backend-specific methods.

## Value

The same token table with an added `rarity` column.

## Details

Rarity quantifies how informative a token is when comparing records. In
**joinery**, rarity is always computed:

- using **one global rarity metric** specified in the strategy,

- **per column**, because each field has its own token distribution,

- **within each block** (if the strategy specifies `block_by`).

The input `tokens` must be the long-format token table returned by
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md),
containing at minimum:

- an ID column,

- a `column` field indicating the source variable,

- a `token` field,

- a `row_id` identifying the originating record,

- and any `block_by` variables required by the strategy.

Backends (e.g., data.frame, data.table, DuckDB relations) may implement
their own methods for this generic, but all must return the same logical
structure: the original token table with an added numeric `rarity`
column.
