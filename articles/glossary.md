# Glossary

joinery has a small vocabulary that the articles and the reference pages
lean on. This page is the single place to look a term up: one short
definition each, and the verb or argument it attaches to. Terms are
ordered alphabetically for lookup. For the order they appear in a real
workflow, read the [getting
started](https://edubruell.github.io/joinery/articles/joinery.md)
article instead.

A note on naming. The word “join” runs through the package because the
work is the woodworker’s: take two pieces cut at different times, on
different machines, and make them meet so cleanly the seam disappears.
The terms below are the measurements and the jigs that get a join to
fit.

------------------------------------------------------------------------

**aIP (asymmetric identification potential).** A measure of how strongly
a shared token points to the same real-world thing, and one that can
differ by direction. A token that is rare in one table but common in the
other carries more weight when you search from the side where it is
rare. That asymmetry is what the “a” stands for. joinery uses aIP only
during calibration:
[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
computes it for each token to help a model judge which candidate pairs
are true matches. The formula comes from Doherr (2023). Specialist,
calibration only.

**Block / blocking (`block_by`).** A stable key, such as a region or an
industry code, that records must agree on before they are compared. It
restricts the comparison to within-block pairs, which cuts cost at a
small recall risk. Use
[`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)
to choose one.

**Candidate / candidate match.** A record pair the search proposes as a
possible match, with a score. Produced by
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md).
A candidate is not yet a confirmed match; the threshold, and optionally
calibration, decide.

**Containment.** A relaxed exact match where one record’s token set is a
subset of another’s, rather than equal. Off by default on
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
(`containment = "off" | "forward" | "bidirectional"`); it over-links on
noisy data.

**Document frequency (df).** How many records a token appears in within
its column and block. High df means common and uninformative. The
`max_token_df` lever drops tokens above a df cap before scoring;
[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
reports df.

**Entity.** A group of records that are the same real-world thing: the
connected component of the match graph. A duplicate group within a
table, or a matched cluster across tables, is an entity. Produced by
[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md).

**Exact strategy.** A strategy that links records only when their token
sets are identical within a block, with no scoring or threshold. Cheap,
robust to empty columns, and the usual front stage of a staged run.
Built by
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md).

**Feedback weighting (`feedback_strength`).** An optional adjustment
that nudges a pair’s score by the proportion of its tokens that matched.
Off by default (0).

**Fuzzy match.** The scored, threshold-based matching of a
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md),
as opposed to the all-or-nothing exact match. It is “fuzzy” because
near-misses still score and can clear the threshold.

**Ledger.** The directed record of every link a staged cross-source
search made, with the stage and direction of each, attached to a
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
result as the `ledger` attribute. The audit trail behind the entity
grouping.

**Match graph / edge list.** The set of scored record pairs viewed as a
graph whose nodes are records and whose edges are matches.
[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)
turns it into entities.

**Rarity.** How informative a token is, computed per column and block.
The default metric is `inverse_freq`. This is the core quantity behind
scoring: rare shared tokens score high. Set with `rarity =` on the
strategy; reported by
[`compute_rarity()`](https://edubruell.github.io/joinery/reference/compute_rarity.md).

**Rarity prefilter (`min_rarity`, `max_token_df`).** Two cut levers
applied to the token table before the overlap join. `min_rarity` floors
the rarity metric; `max_token_df` caps raw document frequency. Both tame
the fan-out from common tokens. Set them from
[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md).

**Representative (`rep`, `rep_by`).** The one record chosen to stand for
an entity (rank 1). Chosen by best score, then a priority column
`rep_by`, then the smallest id.
[`deduplicate_table()`](https://edubruell.github.io/joinery/reference/deduplicate_table.md)
keeps representatives.

**Residual / unmatched.** The records a pass did not match, carried into
a later, looser pass.
[`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
produces them;
[`materialize_records()`](https://edubruell.github.io/joinery/reference/materialize_records.md)
rehydrates a set of ids back into full rows.

**rIP (relative identification potential).** How much of a record’s
identifying power, within one field, sits on a single token. A record’s
tokens split that field’s evidence among them, and a rare token takes a
larger share than a common one: `rIP = rarity / sum(rarity)` over the
record’s tokens in that column. Scoring multiplies each shared token’s
rIP by the column weight and adds them up.

**Score.** A pair’s match strength: the sum over shared tokens of
`rIP * weight`. It sits in `[0, sum(weights)]`, or `[0, 1]` with
normalised weights. Decomposed by
[`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md).

**Smoothing (`smooth_rip_*`).** An optional transform applied to rIP
before scoring (`smooth_rip_identity`, `smooth_rip_log`,
`smooth_rip_offset`, `smooth_rip_softmax`), reshaping how within-record
token weight is distributed. The default is identity.

**Source (`source_by`).** In a multi-source search, the column or
columns recording where each row came from, such as a year or a
register. It lets links be tagged within-source or cross-source and lets
the grouping report how many sources an entity covers.

**Stage.** One pass of a staged workflow with one strategy. Stages run
in order, each on the residual, and optionally the collapsed
representatives, of the last. Run with
[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
or
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md);
diagnosed with
[`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md).

**Strategy.** A declarative object describing how to match: the
per-column preparation pipelines, blocking, rarity metric, weights, and
threshold. Built by
[`search_strategy()`](https://edubruell.github.io/joinery/reference/search_strategy.md),
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md),
or
[`embedding_strategy()`](https://edubruell.github.io/joinery/reference/embedding_strategy.md).
It holds no data and runs nothing itself; the verbs interpret it.

**Threshold.** The minimum score to keep a pair. Either the strategy
default or a per-call override.
[`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
helps set it.

**Token.** A piece of a field’s text after preparation: the unit
everything matches on.
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md),
[`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md),
and the other preparers make them;
[`inspect_tokens()`](https://edubruell.github.io/joinery/reference/inspect_tokens.md)
shows them.

**Token set vs. bag.** joinery scores on token *sets*, the distinct
tokens per record and column, not bags, so repeating a token within one
record does not inflate a score.

**Weight (`weights`).** A column’s share of the total score: a named
numeric vector on the strategy, uniform if omitted. A column’s weight is
also the most it can cost a pair when that column disagrees or is empty.
