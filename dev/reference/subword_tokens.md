# Split text into learned subword tokens

The apply half of the subword pair: place a model fitted by
[`find_subwords()`](https://edubruell.github.io/joinery/dev/reference/find_subwords.md)
in a strategy formula and every record is split into the learned word
pieces: `name ~ normalize_text() + subword_tokens(sw)`.

## Usage

``` r
subword_tokens(text, model)
```

## Arguments

- text:

  A character vector to tokenize.

- model:

  A fitted `Subword_Model` from
  [`find_subwords()`](https://edubruell.github.io/joinery/dev/reference/find_subwords.md).

## Value

A list of character vectors, one per input element, each holding that
element's subword tokens.

## Details

Use it where word tokens are too brittle: compound words
(`"Maschinenbaugesellschaft"` never word-matches `"Maschinenbau"`),
frequent misspellings, or OCR noise. Two spellings of the same name
share most of their pieces even when they share no whole word, so they
can still meet and score. Scoring is unchanged: subword pieces are
ordinary tokens, each weighted by its own rarity.

The model decides how text is split, so pass the text through the same
cleaning steps the model was fitted on (see
[`find_subwords()`](https://edubruell.github.io/joinery/dev/reference/find_subwords.md)).

## See also

[`find_subwords()`](https://edubruell.github.io/joinery/dev/reference/find_subwords.md)
to fit the model;
[`custom_tokens()`](https://edubruell.github.io/joinery/dev/reference/custom_tokens.md),
which this is a named shortcut for;
[`drop_short_tokens()`](https://edubruell.github.io/joinery/dev/reference/drop_short_tokens.md)
to drop single-letter pieces.

Other token generators:
[`custom_tokens()`](https://edubruell.github.io/joinery/dev/reference/custom_tokens.md),
[`generate_ngrams()`](https://edubruell.github.io/joinery/dev/reference/generate_ngrams.md),
[`numeric_tokens()`](https://edubruell.github.io/joinery/dev/reference/numeric_tokens.md),
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md)

## Examples

``` r
# \donttest{
if (requireNamespace("sentencepiece", quietly = TRUE)) {
  firms <- rep(c(
    "maschinenbau mueller", "tischlerei schmidt",
    "holzbau wagner", "metallbau krause"
  ), 40)
  sw <- find_subwords(firms, vocab_size = 60)
  subword_tokens(c("maschinenbaugesellschaft", "holzbau wagner kg"), sw)
}
#> [[1]]
#>  [1] "ma"   "sch"  "inen" "bau"  "g"    "e"    "se"   "ll"   "sch"  "a"   
#> [11] "f"    "t"   
#> 
#> [[2]]
#> [1] "ho"    "lz"    "bau"   "w"     "agner" "k"     "g"    
#> 
# }
```
