# Tokenize Text with a Fitted Tokenizer

Most tokenizers in joinery are plain functions, like
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md).
Some tokenizers instead carry fitted state, for example a subword
vocabulary learned from your corpus. Such a tokenizer is an object, and
`tokenize()` is how that object turns text into tokens. You rarely call
it yourself: place the object in a strategy formula with
[`custom_tokens()`](https://edubruell.github.io/joinery/dev/reference/custom_tokens.md)
and joinery calls `tokenize()` for you.

To bring your own tokenizer, write a `tokenize()` method for its class
with
[`S7::method()`](https://rconsortium.github.io/S7/reference/method.html).
The method must return a list with one element per element of `text`,
each element a character vector of that record's tokens (`character(0)`
for a record with none).

## Usage

``` r
tokenize(tokenizer, text, ...)
```

## Arguments

- tokenizer:

  A fitted tokenizer object.

- text:

  A character vector, one element per record.

- ...:

  Additional arguments passed to methods.

## Value

A list of character vectors, one per element of `text`.

## See also

[`custom_tokens()`](https://edubruell.github.io/joinery/dev/reference/custom_tokens.md),
which puts a tokenizer into a strategy formula.
