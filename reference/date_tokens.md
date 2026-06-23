# Extract date components as tokens

`date_tokens()` parses dates and extracts specified components (year,
month, day) as separate tokens. This is useful for flexible date
matching where you want to match on specific date parts rather than full
dates.

## Usage

``` r
date_tokens(
  x,
  components = c("year", "month", "day"),
  format = NULL,
  orders = c("ymd", "dmy", "mdy")
)
```

## Arguments

- x:

  A character or Date vector containing dates to tokenize.

- components:

  Character vector specifying which date components to extract. Can
  include `"year"`, `"month"`, and/or `"day"`. Defaults to all three.

- format:

  Optional format string for parsing (passed to
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)). If `NULL`
  (default), attempts automatic parsing via lubridate.

- orders:

  Optional character vector of lubridate order specifications (e.g.,
  `c("dmy", "mdy", "ymd")`). Used when `format = NULL`. Defaults to
  `c("ymd", "dmy", "mdy")`.

## Value

A list of character vectors, one per input element. Each vector contains
the requested date components as strings. Unparseable dates return an
empty character vector with a warning.

## Details

Components are returned as zero-padded strings:

- `"year"` – 4-digit year (e.g., `"2023"`)

- `"month"` – 2-digit month (e.g., `"01"`, `"12"`)

- `"day"` – 2-digit day (e.g., `"05"`, `"31"`)

The order of tokens in the output follows the order of `components`.

## See also

[`normalize_date()`](https://edubruell.github.io/joinery/reference/normalize_date.md)
to match whole dates,
[`approximate_date()`](https://edubruell.github.io/joinery/reference/approximate_date.md)
to match on coarser periods.

Other date preparers:
[`approximate_date()`](https://edubruell.github.io/joinery/reference/approximate_date.md),
[`normalize_date()`](https://edubruell.github.io/joinery/reference/normalize_date.md)

## Examples

``` r
date_tokens("2023-12-31")
#> [[1]]
#> [1] "2023" "12"   "31"  
#> 
# list(c("2023", "12", "31"))

date_tokens("31.12.2023", components = c("year", "month"))
#> [[1]]
#> [1] "2023" "12"  
#> 
# list(c("2023", "12"))

date_tokens("12/31/2023", components = "year")
#> [[1]]
#> [1] "2023"
#> 
# list("2023")

date_tokens(c("2023-01-15", "15.06.2023"))
#> [[1]]
#> [1] "2023" "01"   "15"  
#> 
#> [[2]]
#> [1] "2023" "06"   "15"  
#> 
# list(c("2023", "01", "15"), c("2023", "06", "15"))
```
