# Approximate dates by rounding to coarser time units

`approximate_date()` rounds dates to the start of broader time periods
(month, quarter, half-year, year, or decade). This is useful for fuzzy
temporal matching when exact dates may differ slightly but represent the
same general time period.

## Usage

``` r
approximate_date(
  x,
  unit = c("month", "quarter", "half", "year", "decade"),
  format = NULL,
  orders = c("ymd", "dmy", "mdy")
)
```

## Arguments

- x:

  A character or Date vector containing dates to approximate.

- unit:

  Character string specifying the rounding unit. One of:

  - `"month"` – round to first day of month (default)

  - `"quarter"` – round to first day of quarter (Jan 1, Apr 1, Jul 1,
    Oct 1)

  - `"half"` – round to first day of half-year (Jan 1 or Jul 1)

  - `"year"` – round to January 1

  - `"decade"` – round to first year of decade (e.g., 2020-01-01)

- format:

  Optional format string for parsing (passed to
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)). If `NULL`
  (default), attempts automatic parsing via lubridate.

- orders:

  Optional character vector of lubridate order specifications. Used when
  `format = NULL`. Defaults to `c("ymd", "dmy", "mdy")`.

## Value

A character vector of dates in ISO 8601 format (YYYY-MM-DD), rounded to
the start of the specified time unit. Unparseable dates return
`NA_character_` with a warning.

## Details

Rounding always goes to the **start** of the period:

- `"month"`: 2023-03-15 -\> 2023-03-01

- `"quarter"`: 2023-03-15 -\> 2023-01-01 (Q1), 2023-05-20 -\> 2023-04-01
  (Q2)

- `"half"`: 2023-03-15 -\> 2023-01-01 (H1), 2023-08-20 -\> 2023-07-01
  (H2)

- `"year"`: 2023-03-15 -\> 2023-01-01

- `"decade"`: 2023-03-15 -\> 2020-01-01

## See also

[`normalize_date()`](https://edubruell.github.io/joinery/reference/normalize_date.md)
for exact dates,
[`date_tokens()`](https://edubruell.github.io/joinery/reference/date_tokens.md)
to split a date into part tokens.

Other date preparers:
[`date_tokens()`](https://edubruell.github.io/joinery/reference/date_tokens.md),
[`normalize_date()`](https://edubruell.github.io/joinery/reference/normalize_date.md)

## Examples

``` r
approximate_date("2023-03-15", unit = "month")
#> [1] "2023-03-01"
# "2023-03-01"

approximate_date("2023-03-15", unit = "quarter")
#> [1] "2023-01-01"
# "2023-01-01"

approximate_date("2023-08-20", unit = "half")
#> [1] "2023-07-01"
# "2023-07-01"

approximate_date("2023-03-15", unit = "year")
#> [1] "2023-01-01"
# "2023-01-01"

approximate_date("2023-03-15", unit = "decade")
#> [1] "2020-01-01"
# "2020-01-01"

approximate_date(c("2023-01-15", "2023-04-20", "2023-09-10"), unit = "quarter")
#> [1] "2023-01-01" "2023-04-01" "2023-07-01"
# c("2023-01-01", "2023-04-01", "2023-07-01")
```
