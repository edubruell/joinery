# Inspect Tokens for a Specific Column

Extract and examine the tokens generated for a specific column after
applying the preprocessing steps defined in a `Search_Strategy`. Useful
for debugging and understanding how text is tokenized.

## Usage

``` r
inspect_tokens(data, id, strategy, column)
```

## Arguments

- data:

  A data.frame / tibble / data.table (or db table in other backends).

- id:

  Character scalar naming the ID column in `data`.

- strategy:

  A `Search_Strategy` object defining preprocessing steps.

- column:

  \<[`data-masked`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  The column to inspect.

## Value

A backend-specific table showing the tokens generated for the specified
column.
