# Filter out stopwords from token lists

Some tokens carry no matching signal but appear everywhere: legal forms
like `GMBH` or `LTD`, articles, generic words. Because they are common
they create many spurious matches and fan out blocks.
`filter_stopwords()` removes named tokens so matching rests on the
distinctive ones. The comparison is case-insensitive.

## Usage

``` r
filter_stopwords(tokens, stopwords)
```

## Arguments

- tokens:

  A list of character vectors, as produced by
  [`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md).

- stopwords:

  A character vector of tokens to remove (case-insensitive).

## Value

A list of character vectors with the stopwords removed.

## Details

It transforms a token column, so it runs after a token generator such as
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md).

## See also

[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md)
to remove house numbers the same way.

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md),
[`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)

## Examples

``` r
filter_stopwords(list(c("MUELLER", "GMBH")), stopwords = c("gmbh"))
#> [[1]]
#> [1] "MUELLER"
#> 
# list("MUELLER")
```
