# joinery 1.0.1

This is a patch release fixing the M1mac additional-issue check failures
reported by the CRAN team on 2026-07-07 (deadline 2026-07-21).

## The fix

The 43 test failures on the M1mac check machine all came from one call:
DuckDB batch auto-tuning shelled out to `sysctl -n hw.memsize` to read total
RAM, and `sysctl` (in `/usr/sbin`) is not on the `PATH` there, so batch
planning errored and every DuckDB-backed test failed downstream.

The OS probe has been removed entirely. The batch planner now reads the
memory budget from the DuckDB connection's own `memory_limit` setting
(`SELECT current_setting('memory_limit')`), with a conservative fallback
when the setting cannot be read. No system commands are called anywhere in
the package anymore, and the behaviour is identical across platforms.

No other changes.

## Test environments

* local macOS (arm64), R 4.5.3, including a run with `/usr/sbin` removed
  from `PATH` to reproduce the M1mac condition
* win-builder, R-devel and R-release

## R CMD check results

0 errors | 0 warnings | 0 notes locally.
