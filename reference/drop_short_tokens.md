# Drop short tokens from token lists

Removes tokens shorter than `min_nchar` characters from a token column.
Where
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)'s
own `min_nchar` filters length at *tokenisation*, this filters length
*after* a token transform - which is where it matters for the phonetic
encoders
([`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md),
[`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md),
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md))
and
[`generate_ngrams()`](https://edubruell.github.io/joinery/reference/generate_ngrams.md):
those produce short codes, and a 1-2 character code maps to a very large
equivalence class (low distinctiveness), so it behaves as a false-match
magnet. Chain `drop_short_tokens()` after the encoder to keep only the
discriminative codes.

Operates on the list-of-character token vectors produced by earlier
steps, mirroring
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
/
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md).

## Usage

``` r
drop_short_tokens(tokens, min_nchar = 2)
```

## Arguments

- tokens:

  A list of character vectors.

- min_nchar:

  Whole number; tokens with fewer than this many characters are dropped.
  Default `2`.

## Value

A list of character vectors with short tokens removed.

## See also

[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
and
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md)
for the same list-column idea with other drop rules;
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)
for the same length cut applied at tokenisation instead.

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md),
[`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)

## Examples

``` r
drop_short_tokens(list(c("BAU", "AG", "X")))
#> [[1]]
#> [1] "BAU" "AG" 
#> 
# list(c("BAU", "AG"))  # the 1-char token is dropped at the default min_nchar = 2

# keep only Cologne codes of 4+ digits (drops the collision-prone short class)
drop_short_tokens(as_cologne(list(c("Bülau", "Mertens"))), min_nchar = 4)
#> [[1]]
#> [1] "67268"
#> 
# list("67268")
```
