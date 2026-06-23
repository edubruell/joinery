# Calibrating a false-positive filter

A token matcher is built to be forgiving. It scores a pair of records by
how much rare word mass they share, so it finds the real pairs even when
the spelling drifts or the word order changes. That same forgiveness
lets a few wrong pairs through: two different workshops that happen to
share a common surname, a generic trade word, or a venue name. Retrieval
gets you the candidates that look alike. The job left over is to throw
out the ones that look alike but are not the same.

That job is calibration. You take a few hundred pairs, label each one by
hand as a true match or not, train a small model on the features of
those pairs, and use it to score every other pair. The model gives each
pair a probability of being a true match. Then you decide where to draw
the line.

This article walks the whole loop on the workshop register data, and
spends most of its length on the decision that has no default answer:
**where to draw that line depends on what you do with the linked data
next.**

``` r

library(joinery)
library(dplyr)
library(data.table)
```

## The data

Two tables carry the example. `workshop_register` is a clean guild roll
of 1,052 woodworking workshops; `workshop_listings` is a messier
external directory of 894 entries, most of which point back to a
register row through their `actual_link` column. We searched one against
the other and got candidate pairs. Some are right, some are not.

The labelled set ships ready to use. `match_labels_example` holds 931
candidate pairs (two rows each, one for the listing and one for the
register row it was paired with), with an `equal` column that marks each
pair as a true match (`1`) or a false one (`0`). It is the frozen output
of the manual labelling loop shown below: 711 of the pairs are true, 220
are false, and the false ones cluster in the hard cases, workshops that
share a name but not an owner.

``` r

data(workshop_register)
data(workshop_listings)
data(match_labels_example)

# two rows per pair, so count distinct pairs
as.data.table(match_labels_example)[, .(pairs = uniqueN(match_id)), by = equal]
#>    equal pairs
#>    <int> <int>
#> 1:     1   711
#> 2:     0   220
```

We rebuild the search so we have a live match result to work from. A
loose threshold keeps the leaky candidates in, which is what we want
here: the filter, not the threshold, is what removes them.

``` r

strat <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.30
)

m <- search_candidates(
  workshop_listings, workshop_register,
  base_id   = "listing_id",
  target_id = "reg_no",
  strategy  = strat
)
```

## Why retrieval is not enough

Look at the score distribution split at the threshold. A healthy chunk
of the mass sits just above the cut, in the borderline band where true
and false pairs are mixed together. No single score cleanly separates
them, which is the whole reason a trained filter earns its keep.

``` r

ov <- summarise_matches(m, threshold = 0.5)
ov
#> 
#> ── Match_Overview (candidates) ─────────────────────────────────────────────────
#> n_pairs_or_groups: "965" n_records_involved: "1461"
#> coverage: base=NA target=NA
#> score summary
#> min: 0.300
#> q1: 0.667
#> median: 0.808
#> mean: 0.828
#> q3: 1.000
#> max: 1.000
#> candidates-per-record (top 5)
#> 1 candidate(s): 564 record(s)
#> 2 candidate(s): 68 record(s)
#> 3 candidate(s): 79 record(s)
#> 4 candidate(s): 7 record(s)
#> ! 12.0% of base records have >= 3 candidate matches; consider `max_candidates` or raising threshold.
#> ! median top-1 vs top-2 score gap is 0.000; matches are weakly decisive, consider raising threshold or `feedback_strength`.
```

A concrete leak: a listing for one “Walker Joinery” gets paired with a
*different* Walker Joinery in the same postcode area, run by a different
person. Both are real workshops. The shared surname and trade put them
in the same block and gave them a respectable score. Only the
surrounding detail tells them apart, and that is exactly the signal a
feature table exposes.

## Features: what makes a pair convincing

[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
turns each candidate pair into one row of numbers a model can learn
from. It needs the strategy and both source tables so it can recompute
the per-pair detail.

``` r

feats <- match_features(
  m, strat,
  base   = workshop_listings, id = "listing_id",
  target = workshop_register, target_id = "reg_no"
)

dim(as.data.table(feats))
#> [1] 965  46
```

That is one row per candidate pair and several dozen columns. You rarely
read them by hand; the model does. But it helps to know the three groups
they fall into, because they mirror how a person decides whether two
records are the same workshop. One column from each group, for the first
few pairs:

``` r

as.data.table(feats)[, .(match_id, score,
                         matched    = m_workshop_1,
                         reg_extra  = f_proprietor_1,
                         list_extra = s_workshop_1,
                         rivals     = cnt)] |>
  head()
#>    match_id score   matched reg_extra list_extra rivals
#>       <int> <num>     <num>     <num>      <num>  <int>
#> 1:        1     1 0.5908422        NA         NA      2
#> 2:        2     1 0.5908422        NA         NA      2
#> 3:        3     1 1.0000000 0.4713661         NA      1
#> 4:        4     1 1.0000000        NA         NA      1
#> 5:        5     1 1.0000000        NA         NA      1
#> 6:        6     1 1.0000000        NA         NA      1
```

- **The words that caused the match** (`m_workshop_*`,
  `m_proprietor_*`). The words the two records share, each weighted by
  how rare it is. A shared rare surname is strong evidence; a shared
  “joinery” is weak. This is the positive signal.
- **The words only one side had** (`f_*` for the register side, `s_*`
  for the listing side). Surplus words the other record never mentions.
  A pair where each side carries several unexplained words is weaker
  than one where the words line up.
- **The shape of the block** (`cnt`, `icnt`, `ipos`, `scnt`, `rcnt`).
  How many register rows competed for this listing, and where this pair
  ranked among them. A pair that won against three rivals is a different
  proposition from one that was the only candidate.

Most of these columns are mostly empty, and that is expected rather than
a problem. A slot like `f_proprietor_2` holds the *second* surplus word
on the register side; a pair with no second surplus word leaves it `NA`.
An `NA` here means “this pair had nothing for that slot”, not a value
that went missing.
[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
reads `NA` as a plain zero (its `na_fill` argument), so an absent word
contributes nothing instead of breaking the fit.

The block-shape group only carries signal when listings actually face
competing candidates. In this data about one listing in five draws more
than one register row, which is enough for those features to matter.

## Label a sample (the real loop)

The labelled set did not appear from nowhere. You build it by sampling
pairs, exporting them to a CSV, marking each one by hand, and reading
the result back. joinery samples where the decision is hardest, in the
borderline band and in blocks where several candidates compete:

``` r

sample <- sample_matches(m, mode = "borderline", n = 200,
                         stratify_by = "stage", expand_to_block = TRUE)

export_for_labelling(sample, "to_label.csv")   # edit the equal column by hand
labels <- import_labels("to_label.csv")         # read it back, ready to fit
```

`match_labels_example` is exactly that output, frozen so the rest of
this article runs without a manual step. From here on we use it
directly.

## Fit the filter

[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
joins the features to the labels and fits a logistic model. The default
is a plain `glm`, which keeps the baseline path free of extra
dependencies. It reports how many labelled rows it trained on and the
class balance.

``` r

fm <- fit_filter(feats, match_labels_example, model = "logistic")
fm
#> <joinery::Filter_Model>
#> backend : glm
#> model_class : glm
#> predictors (42) : score, cnt, icnt, ipos, scnt, rcnt, r1, r2
#> ... +34 more
#> training_n : 442
#> class_balance : 0.756 (share of equal == 1L)
#> class_weighted : FALSE
```

The labelled set leans toward true matches (the class balance the print
reports). If the false pairs are the rare class you most want to catch,
pass `class_weighted = TRUE` to weight the two classes evenly while
fitting. We keep the plain unweighted fit here because it already
separates the classes well on this data.

If you prefer a tidymodels workflow,
[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
also accepts a `parsnip` model spec or a `workflows` object, and
[`joinery_recipe()`](https://edubruell.github.io/joinery/reference/joinery_recipe.md)
builds a recipe with the id and outcome roles already tagged. Those
packages stay in `Suggests`, so the `glm` path above needs nothing extra
installed.

## Choosing where to draw the line

This is the decision that matters, and the one with no universal answer.

[`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
scores every pair and, by default, picks the threshold that best
balances two quantities (Youden’s J). The first is **precision**, the
share of the pairs it keeps that are really matches. The second is
**recall**, the share of all the true matches it manages to keep. On
this data the balanced line lands near 0.82, and it favours precision:
of the pairs it keeps almost all are real, but it drops a number of true
matches to get there.

``` r

cm_balanced <- apply_filter(feats, fm)
cm_balanced
#> 
#> ── Calibrated_Matches ──────────────────────────────────────────────────────────
#> <joinery::Calibrated_Matches>
#> threshold : 0.7237 (method: youden_j)
#> n_rows : 965
#> predicted_tp == 1: 616
#> predicted_tp == 0: 349
#> tp_prob quantiles: 0.000 / 0.016 / 0.972 / 0.998 / 1.000
```

You can ask for a different operating point. The `threshold_rule`
argument trades the two kinds of error against each other directly:

- `"target_recall"` keeps the strictest line that still recovers a set
  fraction of the true matches. Use it when missing a real link is the
  expensive mistake.
- `"cost_weighted"` minimises
  `cost_ratio * (missed links) + (wrong links)`. A `cost_ratio` above 1
  says a missed link hurts more than a wrong one.

``` r

cm_recall <- apply_filter(feats, fm, threshold_rule = "target_recall",
                          target_recall = 0.95)
cm_recall
#> 
#> ── Calibrated_Matches ──────────────────────────────────────────────────────────
#> <joinery::Calibrated_Matches>
#> threshold : 0.6886 (method: target_recall)
#> n_rows : 965
#> predicted_tp == 1: 620
#> predicted_tp == 0: 345
#> tp_prob quantiles: 0.000 / 0.016 / 0.972 / 0.998 / 1.000
```

Both are defensible. Which one is right is not a property of the
matcher. It is a property of the question you are about to ask of the
data.

### The error you should fear depends on the analysis

A **false positive** is an over-link: two records fused that belong to
different workshops. A **false negative** is a missed link: a true pair
the filter rejected. Every analysis pays for these two mistakes
differently, and sometimes in opposite directions.

| Downstream analysis | Over-link (false positive) does | Missed link (false negative) does | Which to favour |
|----|----|----|----|
| **How many workshops are active in an area** | merges two real firms, undercount | splits one firm in two, overcount | balanced: the two biases partly cancel in a level |
| **Entry, exit, and survival over time** | erases one firm’s birth and another’s death, churn looks too low | invents a phantom exit and re-entry, churn looks too high | no easy call: both directions bias, oppositely |
| **Attaching an external variable to each firm** | wrong record linked, wrong values, measurement error | row dropped; selection bias if hard-to-link firms differ | precision, unless non-linkage is itself informative |
| **Following the same firms before and after an event** | another firm’s outcomes leak into the trajectory | a treated firm drops out, lost units and attrition | recall, while watching for contamination |
| **Cleaning a register to count establishments** | erases a real establishment, deflates the count | keeps a duplicate, double-counts | a conservative merge, precision |

The first row is the gentle case. If you only want a count of distinct
workshops in a level, over-linking and under-linking push the number in
opposite directions and partly cancel, so a balanced cut is fine.

### Why entry and exit is the hard one

Now follow the same workshops across years instead of counting them
once. The package ships `workshop_panel`, a pooled multi-year version of
the same universe (847 rows, 320 firms, 2019 to 2023), with the
cross-year links as ground truth. Linking it lets you read each firm’s
birth year, death year, and how long it survived. Here the two errors do
not cancel. They bias the answer in opposite directions, and there is no
operating point that is safe for both.

- **Over-link.** Fuse two distinct “Walker Joinery” workshops into one
  firm. That single wrong link erases one firm’s entry and another’s
  exit. The merged firm looks longer-lived than either real one, and the
  area’s churn looks lower than it is. A recall-favouring cut admits
  exactly these same-name pairs, so a recall-favouring cut **understates
  churn and overstates survival.**
- **Under-link.** A workshop that relocated, or whose name drifted, is
  not linked across the move. That manufactures a fake exit in one year
  and a fake entry the next. Churn looks too high, survival too short. A
  precision-favouring cut drops these borderline cross-year links, so a
  precision-favouring cut **overstates churn and understates survival.**

So for an entry-and-exit study you cannot reach for a default. The
project’s usual habit of favouring recall is right for a within-table
cleanup where a later, looser pass re-bridges what an early strict pass
missed. For a churn estimate that same habit quietly bakes in a bias
toward low turnover. The honest options are to pick the bias you can
defend and correct for, or to stop hard-thresholding at all and carry
the probability forward (the “Beyond a single line” section below). The
full multi-year workflow, and reading entry and exit off the resulting
entities, is the [article on matching across years and
sources](https://edubruell.github.io/joinery/articles/staged.md).

## Apply the chosen line

Once you have an operating point,
[`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
writes a `tp_prob` (the probability the pair is a true match) and a
`predicted_tp` flag onto the result. Pass the original matches in
`matches =` to broadcast both onto every row of the match table, ready
to feed the rest of your pipeline:

``` r

cm <- apply_filter(feats, fm, matches = m)
head(as.data.table(cm@matches)[, .(match_id, source, score, tp_prob, predicted_tp)])
#>    match_id source score     tp_prob predicted_tp
#>       <int> <char> <num>       <num>        <int>
#> 1:        1   base     1 0.006985289            0
#> 2:        1 target     1 0.006985289            0
#> 3:        2   base     1 0.006985289            0
#> 4:        2 target     1 0.006985289            0
#> 5:        3   base     1 0.979645996            1
#> 6:        3 target     1 0.979645996            1
```

The two operating points keep different sets. The balanced cut keeps the
pairs it is sure about and rejects almost every false one; the
recall-favouring cut keeps nearly all the true matches at the cost of
letting a few more false ones through. Sliced by which kind of name
collision they came from, the balanced filter rejects every one of the
planted same-name-same-block traps and the large majority of the
register duplicates. The mistakes it removes are exactly the ones that
would corrupt a firm count or a churn estimate.

## Evaluate honestly

A filter that reports a probability is only useful if that probability
means what it says.
[`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)
checks this. It returns a reliability table (in each probability band,
does the observed share of true matches line up with the predicted
probability), a Brier score and log-loss (lower is better), and the full
threshold sweep.

``` r

cal <- calibrate(cm_balanced)
cal
#> 
#> ── Filter_Calibration ──────────────────────────────────────────────────────────
#> n_eval: "442" class_balance: "0.756"
#> threshold: "0.7237" brier: "0.0284" log_loss: "0.0870"
#> confusion
#> equal=0 pred=0: 105 pred=1: 3
#> equal=1 pred=0: 17 pred=1: 317
#> ! filter was fit on only 442 labelled pairs; consider expanding the labelled sample to >= 500 for stable calibration.
```

``` r

rel <- as.data.frame(cal@reliability)
plot(rel$mean_pred, rel$obs_pos,
     type = "b", pch = 19,
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Mean predicted probability",
     ylab = "Observed share of true matches",
     main = "Reliability")
abline(0, 1, lty = 2)
```

![](calibration_files/figure-html/reliability-plot-1.png)

Points on the dashed line mean the probabilities are honest: pairs the
model calls 90% likely are true about 90% of the time. Had we trained on
only a couple of hundred labels,
[`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)
would have added a low-sample warning, because the reliability estimate
itself gets noisy when the labelled set is small. This is the point
where joinery goes past simply counting hits and misses: it tells you
whether to trust the number before you build a threshold on it.

## Beyond a single line

A global threshold is the simplest tool, not the only one. Ordered from
cheapest to most careful:

1.  **One global threshold.** What we did above. Pick balanced,
    recall-favouring, or precision-favouring from the sweep, guided by
    the downstream table.
2.  **A threshold per stage.** If you matched in stages, the exact-match
    stage scores its pairs at 1.0 and is almost never wrong. Threshold
    only the fuzzy stages. The `stage` column is on every result, so
    this is usually the cheapest large gain.
3.  **Carry the probability, do not cut.** Keep `tp_prob` as a link
    weight instead of a yes-or-no, and let the uncertainty flow into the
    downstream estimate, for instance by weighting observations or by
    imputing over uncertain links. This is the rigorous answer for the
    entry-and-exit case, where any hard cut bakes in a directional bias.
4.  **A review queue.** Accept the confident matches, reject the
    confident non-matches, and route the borderline band to a person
    with `sample_matches(mode = "borderline")`. The practical hybrid
    when the analysis is high-stakes and more labels are cheap to get.

## Sanity-check: does the conclusion move with the cut?

Picking an operating point is a judgement call, so do not let the whole
result rest on it silently. The cheapest insurance is to run your actual
downstream analysis under more than one matching regime and see whether
the answer holds.

A natural pair to compare is a strict, exact-only linkage against the
permissive, calibrated one. Exact matching is almost never wrong, but it
recovers only the easy cases and misses the harder ones: the movers, the
drifters, the sound-alikes. If those hard-to-link workshops differ
systematically from the easy ones, an exact-only sample is a *selected*
sample, and an estimate built on it can be biased even though every link
in it is correct. The permissive linkage recovers those harder cases, at
the cost of admitting a few wrong links.

So compute the number you actually care about both ways. If the strict
and permissive linkages give materially different answers, the gap is
telling you that linkage error is moving your estimate, and you need to
either defend the operating point or carry the uncertainty forward
rather than hard-cut. If they agree, that agreement is real reassurance:
the conclusion is robust to how permissive you were, and the choice of
line stops being something to worry about.

## Where to look next

- The matches this article cleans were built in the [features
  article](https://edubruell.github.io/joinery/articles/features.md);
  start there for the search itself.
- The full multi-year workflow that the entry-and-exit example sketches,
  including how to read birth and death years off the linked entities,
  is the [article on matching across years and
  sources](https://edubruell.github.io/joinery/articles/staged.md).
- Carrying linkage uncertainty all the way into a downstream model,
  rather than thresholding, is on the roadmap beyond the 1.0 release.
