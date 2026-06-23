# Normalize dates to ISO 8601 format (YYYY-MM-DD)

The same day is written `"31.12.2023"`, `"12/31/2023"`, or
`"2023-12-31"` depending on who typed it. `normalize_date()` parses
these mixed formats and rewrites them to one ISO 8601 string
(`YYYY-MM-DD`), so a date column matches on the day it names rather than
on how it was formatted. It recognizes European (DD.MM.YYYY), American
(MM/DD/YYYY), and ISO-style inputs.

## Usage

``` r
normalize_date(x, format = NULL, orders = c("ymd", "dmy", "mdy"))
```

## Arguments

- x:

  A character or Date vector containing dates to normalize.

- format:

  Optional format string for parsing (passed to
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)). If `NULL`
  (default), attempts automatic parsing via multiple common formats.

- orders:

  Optional character vector of lubridate order specifications (e.g.,
  `c("dmy", "mdy", "ymd")`). Used when `format = NULL`. Defaults to
  `c("ymd", "dmy", "mdy")`.

## Value

A character vector of dates in ISO 8601 format (YYYY-MM-DD). Unparseable
dates return `NA_character_` with a warning.

## Details

Returns text. For matching on individual date parts (year only, year and
month) use
[`date_tokens()`](https://edubruell.github.io/joinery/reference/date_tokens.md);
to deliberately blur near-dates together use
[`approximate_date()`](https://edubruell.github.io/joinery/reference/approximate_date.md).

When `format` is provided, uses `as.Date(x, format)` directly. When
`format = NULL`, tries
[`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
with the specified `orders` to handle mixed formats flexibly.

## See also

Other date preparers:
[`approximate_date()`](https://edubruell.github.io/joinery/reference/approximate_date.md),
[`date_tokens()`](https://edubruell.github.io/joinery/reference/date_tokens.md)

## Examples

``` r
normalize_date("31.12.2023")
#> [1] "2023-12-31"
# "2023-12-31"

normalize_date("12/31/2023")
#> [1] "2023-12-31"
# "2023-12-31"

normalize_date(c("2023-01-15", "15.01.2023", "01/15/2023"))
#> [1] "2023-01-15" "2023-01-15" "2023-01-15"
# c("2023-01-15", "2023-01-15", "2023-01-15")

normalize_date("31-12-2023", format = "%d-%m-%Y")
#> [1] "2023-12-31"
# "2023-12-31"
```
