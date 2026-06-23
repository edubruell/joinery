# Score Embedding Pairs Using Cosine Similarity

Compute cosine similarity scores between base and target embeddings.
This is a pure scoring function that operates on pre-computed
embeddings.

## Usage

``` r
score_embeddings(base_embeddings, target_embeddings, strategy, ...)
```

## Arguments

- base_embeddings:

  A table with columns: `id` and `embedding`.

- target_embeddings:

  A table with columns: `id` and `embedding`.

- strategy:

  An `Embedding_Strategy` object (used for normalization settings).

- ...:

  Additional arguments passed to backend-specific methods.

## Value

A backend-specific table with columns: `base_id`, `target_id`, `score`.
