# Create an Embedding Strategy

Construct an `Embedding_Strategy` object for semantic matching using
embeddings. This is a distinct strategy type from token-based strategies
created with
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md).

Embedding strategies:

- Represent entire records as embedding vectors

- Use cosine similarity for scoring

- Support blocking variables to restrict comparisons

- Require the tidyllm package for embedding computation

## Usage

``` r
embedding_strategy(
  columns = NULL,
  embedding_model,
  threshold,
  collapse_sep = " ",
  normalize = TRUE,
  batch_size = 1000,
  block_by = NULL
)
```

## Arguments

- columns:

  Character vector of column names to embed, or NULL (default) to use
  all non-id character-like columns.

- embedding_model:

  A tidyllm provider object (e.g.,
  `ollama(.model = "mxbai-embed-large")`). This is passed directly to
  tidyllm's
  [`embed()`](https://edubruell.github.io/tidyllm/reference/embed.html)
  function.

- threshold:

  Numeric scalar in (0, 1). Cosine similarity threshold for filtering
  matches.

- collapse_sep:

  Character scalar. Separator used when joining multiple columns into a
  single text string. Default is " ".

- normalize:

  Logical scalar. If TRUE (default), apply L2 normalization to
  embeddings before computing cosine similarity.

- batch_size:

  Numeric scalar. Number of records to process per batch when computing
  embeddings. Default is 1000.

- block_by:

  Character vector of blocking variable names, or NULL (default). When
  specified, comparisons are only made within matching blocks.

## Value

An `Embedding_Strategy` S7 object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(tidyllm)

# Create an embedding strategy using Ollama
emb_strat <- embedding_strategy(
  columns = c("name", "address"),
  embedding_model = ollama(.model = "mxbai-embed-large"),
  threshold = 0.85
)

# Use in multi-stage workflow
results <- multi_stage_search(
  base_table = customers_a,
  target_table = customers_b,
  base_id = "id_a",
  target_id = "id_b",
  strategies = list(
    token_stage = search_strategy(name ~ normalize_text() + word_tokens()),
    semantic_stage = emb_strat
  )
)
} # }
```
