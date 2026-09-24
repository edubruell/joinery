# Learn a subword vocabulary from your text

Word tokens fail quietly on compound words and misspellings:
`"Maschinenbaugesellschaft"` and `"Maschinenbau GmbH"` share no word, so
they never meet in a word-token match. Subword tokens fix this by
learning, from your own data, a vocabulary of frequent word pieces and
splitting every record into those pieces. Two spellings of the same name
then share most of their pieces even when they share no whole word.

`find_subwords()` is the learning step. Run it once on the text of a
column (after the same cleaning you use in your strategy), then place
the fitted model in the strategy formula with
[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md):

    sw <- find_subwords(normalize_text(register$name))
    strat <- search_strategy(
      name ~ normalize_text() + subword_tokens(sw)
    )

The fitted model is self-contained: you can save it with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and reuse it in a
later session, and the same model always splits text the same way.

## Usage

``` r
find_subwords(
  x,
  vocab_size = 500L,
  algorithm = c("bpe", "unigram"),
  keep_boundary = FALSE,
  sample_n = NULL,
  columns = NULL
)
```

## Arguments

- x:

  The training text: a character vector, or a table (data frame, tibble,
  data.table, DuckDB table) together with `columns`.

- vocab_size:

  Number of subword pieces to learn. Default `500`.

- algorithm:

  `"bpe"` (default) or `"unigram"`, the two learning algorithms
  SentencePiece offers. Start with `"bpe"`; try `"unigram"` if its
  splits look better on your data.

- keep_boundary:

  Keep the word-boundary marker on each piece? The default `FALSE`
  strips it, so the same piece matches whether it starts a word or not;
  that is usually what record linkage wants.

- sample_n:

  Optional cap on the number of rows used for training. On a corpus of
  millions of rows a uniform sample of a few hundred thousand fits an
  equivalent vocabulary in a fraction of the time.

- columns:

  For table input: character vector naming the text column(s) to train
  on.

## Value

A fitted `Subword_Model`, ready for
[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md).

## Details

Fit the model on the text as the strategy will see it: apply the same
cleaning steps (for example
[`normalize_text()`](https://edubruell.github.io/joinery/dev/reference/normalize_text.md))
to the training text that precede
[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md)
in the formula. Training on raw text and applying to cleaned text (or
the other way round) degrades the splits.

`vocab_size` sets how fine the splits are. Small vocabularies split
aggressively into short pieces; large ones keep frequent words whole and
only split rare ones. The default of 500 suits mid-sized name corpora;
large corpora often support 1000 to 8000. On a very small corpus the
fitted vocabulary can come out smaller than requested; the model records
the size actually fitted.
[`rarity_distribution()`](https://edubruell.github.io/joinery/dev/reference/rarity_distribution.md)
on the prepared tokens is the read for judging the result.

Very frequent single-character pieces are normal and mostly harmless:
they carry almost no rarity, so they contribute little to a score.
Compose `subword_tokens(sw) + drop_short_tokens(2)` to drop them
outright.

## See also

[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md)
to use the model in a strategy;
[`find_stopwords()`](https://edubruell.github.io/joinery/dev/reference/find_stopwords.md)
for the same fit-then-apply pattern on stopwords.

## Examples

``` r
# \donttest{
if (requireNamespace("sentencepiece", quietly = TRUE)) {
  firms <- rep(c(
    "maschinenbau mueller", "tischlerei schmidt",
    "holzbau wagner", "metallbau krause"
  ), 40)
  sw <- find_subwords(firms, vocab_size = 60)
  sw
  subword_tokens("maschinenbaugesellschaft mueller", sw)
}
#> [[1]]
#>  [1] "ma"   "sch"  "inen" "bau"  "g"    "e"    "se"   "ll"   "sch"  "a"   
#> [11] "f"    "t"    "mue"  "ller"
#> 
# }
```
