# format(Match_Overview) snapshot is stable (duplicates)

    Code
      cat(format(res), sep = "\n")
    Output
      <joinery::Match_Overview> (duplicates)
      
      n_pairs_or_groups: 2   n_records_involved: 5
      coverage: base=50.0%   target=NA
      score summary:
        min=0.780  q1=0.800  median=0.880  mean=0.862  q3=0.900  max=0.950
      
      cluster size distribution (top 5):
        size 2: 1 cluster(s)
        size 3: 1 cluster(s)
        max_cluster_size=3   pct_records_in_cluster=50.0%

# format(Match_Overview) snapshot is stable (candidates with recommendations)

    Code
      cat(format(res), sep = "\n")
    Output
      <joinery::Match_Overview> (candidates)
      
      n_pairs_or_groups: 4   n_records_involved: 5
      coverage: base=NA   target=NA
      score summary:
        min=0.300  q1=0.712  median=0.885  mean=0.755  q3=0.927  max=0.950
      
      candidates-per-record distribution (top 5):
        1 candidate(s): 1 record(s)
        3 candidate(s): 1 record(s)
      
      recommendations:
        ! 50.0% of base records have >= 3 candidate matches; consider `max_candidates` or raising threshold.
        ! median top-1 vs top-2 score gap is 0.030; matches are weakly decisive, consider raising threshold or `feedback_strength`.
        ! 50.0% of base records have >= 3 candidate matches; once you have a few hundred labelled pairs, `calibrate_matches()` can re-rank them.

