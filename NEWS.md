# joinery 0.3.0

## Phase 3: SearchEngine Heuristics

This release implements advanced matching heuristics that significantly improve accuracy and robustness.

### New Features

* **rIP Smoothing** — Four smoothing methods for token weights:
  - `smoothing(method = "log")` — Log transformation
  - `smoothing(method = "softmax", temperature = 1.0)` — Softmax with temperature
  - `smoothing(method = "offset", alpha = 0.1)` — Additive smoothing
  - `smoothing(method = "none")` — No smoothing (default)
  
* **Containment** — Control maximum matches per record:
  - `max_candidates` parameter limits top-N matches
  - Prevents one-token overmatching
  - Works with threshold filtering
  
* **Feedback Weighting** — Penalize low token overlap:
  - `feedback_strength` parameter (0-1) controls intensity
  - Reduces noise in partial matches
  - Rewards comprehensive token overlap

### DuckDB Backend

* Unified `.score_pairs_sql()` helper consolidates scoring logic
* All Phase 3 features supported in DuckDB backend
* Used by both `detect_duplicates()` and `search_candidates()`

### Backend Improvements

* Both data.table and DuckDB backends support all Phase 3 features
* Full test coverage for all smoothing, containment, and feedback methods
* 454 tests passing

---

# joinery 0.2.0

## Phase 2: DuckDB Backend

* Full DuckDB backend implementation
* Scalable processing of datasets up to 50M rows
* Batch-based processing with R preprocessing pipeline
* Feature parity between data.table and DuckDB backends
* All core generics working on both backends

---

# joinery 0.1.0

* Initial release
* data.table backend
* Token-based record linkage
* Basic preprocessing pipeline
* S7 class system
