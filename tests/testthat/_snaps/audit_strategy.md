# format() snapshot is stable (clean fixture)

    Code
      cat(format(res), sep = "\n")
    Output
      <joinery::Strategy_Audit>
      
      n_records: 20
      
      column token stats:
        first_name: 20 tokens (20 unique, 100.0% unique, na_rate=0.0%, avg_per_record=1.00)
        last_name: 20 tokens (20 unique, 100.0% unique, na_rate=0.0%, avg_per_record=1.00)
      
      column rarity stats (p05/p25/p50/p75/p95):
        first_name: 1.0000 / 1.0000 / 1.0000 / 1.0000 / 1.0000  (low_rarity=0.0%)
        last_name: 1.0000 / 1.0000 / 1.0000 / 1.0000 / 1.0000  (low_rarity=0.0%)
      
      est_comparisons: 190

