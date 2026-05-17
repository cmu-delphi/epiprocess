# epiprocess: Domain Context

## Glossary

### epi_archive

An **epi_archive** is a bitemporal data store: for every `(geo_value, other_keys..., time_value)` (the **epikey-time**) there may be many rows, one per `version`. It tracks the full revision history of public-health data as it gets republished over time.

The unique key is:

```r
c("geo_value", other_keys..., "time_value", "version")
```

The non-version subset, `c("geo_value", other_keys..., "time_value")`, identifies the epikey-time.

### Backend abstraction

`epi_archive` storage can be backed by:

- **data.table** — original implementation; in-memory, mutate-by-reference, keyed indexes. Archives carry subclass `epi_archive_dt`.
- **duckplyr** — DuckDB-backed lazy frame, for larger data and engine-optimized query plans. Archives carry subclass `epi_archive_duck`.

`tsibble` was considered and rejected: it requires one row per `(key, index)`, which conflicts with the bitemporal model. A pure dplyr/tibble backend may be useful later, but is not a near-term goal.

## Backend abstraction design

The abstraction lives at the **dplyr verb level**. Backends expose a handle that dplyr verbs operate on; archive algorithms should be written as dplyr pipelines plus a small number of domain primitives. Custom verbs such as `archive_distinct()` or `archive_mutate()` were rejected as reinventing dplyr with worse coverage.

### Per-archive dispatch

Each archive carries its backend as an S3 subclass:

```r
c("epi_archive_dt", "epi_archive")
c("epi_archive_duck", "epi_archive")
```

Accessors are S3 generics with backend methods. This supports mixing backends in one R session, e.g. small in-memory fixtures alongside a large DuckDB-backed archive.

### Construction

Backend choice is explicit at construction time:

- `as_epi_archive(data, ...)` — data.table-backed, original behavior.
- `as_duckdb_epi_archive(data, ...)` — duckdb/duckplyr-backed, using the regular eager construction/validation/compactification pipeline before uploading to duckdb.
- `as_duckdb_epi_archive_from_duck(duck_table, ...)` — wraps an existing duckplyr table, or an object `duckplyr::as_duckdb_tibble()` can convert, without first collecting the full table through R. This path expects canonical archive columns and does not perform signal-format conversion or compactification.

A constructor argument like `as_epi_archive(data, backend = "duckdb")` was rejected: separate factories make return type, lifecycle, and performance characteristics visible at call sites.

### The `$DT` public field

`$DT` remains the data.table backend's documented power-user escape hatch. The duck backend exposes `$duck` instead. There is intentionally no uniform `$data` field: eager and lazy semantics differ enough that a shared name would invite misleading assumptions.

Backend-neutral package code and behavior-level tests should use accessors rather than `$DT`/`$duck`.

## Accessor contract

The abstraction is **dplyr verbs + domain primitives**. Dplyr handles most archive algorithms; domain primitives cover operations that dplyr cannot express cleanly or portably across backends.

### Core accessors

- `archive_data(x)` — backend-native dplyr handle. DT: `dtplyr::lazy_dt(x$DT)`. Duck: `x$duck`.
- `archive_set_data(x, data)` — replace storage with a tabular result. DT materializes and re-keys; duck stores a lazy reference. Does not validate; call validation separately when needed.
- `archive_tbl(x)` — eager tibble. **Load-bearing** wherever code needs a real data frame: compactification, `vec_split()`, validation, slide chunking, snapshot comparisons, and tidyeval contexts requiring `.env$`. The public `tibble::as_tibble()` method for `epi_archive` uses this to materialize full archive history backend-neutrally.
- `archive_col(x, col)` — bare vector. On lazy backends this forces materialization of that column; prefer narrower helpers for aggregate-only call sites.
- `archive_col_range(x, col)` — length-2 `range()`-style summary. Duck pushes this down as a single aggregate query and preserves R `range()` missing-value semantics for the supported no-`na.rm` contract.
- `archive_colmask(x)` — 0-row tibble with the archive schema; useful for tidyselect/data-mask operations that only need column names.
- `archive_colnames(x)`, `archive_nrow(x)`, `archive_ncol(x)` — schema/size helpers.
- `archive_filter(x, ...)` — backend-preserving predicate filter; prefer this over precomputing logical masks with `archive_col()` so lazy backends can push predicates down.
- `archive_filter_rows(x, rows)` — backend-preserving row filter for call sites that already have an eager logical mask.
- `archive_deep_copy(x)` — independent copy preserving backend.
- `archive_col_is_factor(x, col)` — factor detection. Duck always returns `FALSE` because DuckDB does not preserve R factor/ordered columns.
- `archive_columns_as_list(x)` — shallow list of column references; used by tidyeval code that needs pointer-equality between input and output columns.
- `archive_any_duplicated_key(x)` — duplicate-key detector. Returns `TRUE` if any key is duplicated, otherwise `FALSE`; it deliberately does not expose backend-specific details such as first duplicate row index or duplicate-group count.

### Domain primitives

- `archive_locf_join(left, right, by)` — rolling join with last-version-carried-forward semantics. The last `by` column is the LOCF axis. DT/default uses data.table roll join; duck uses dplyr rolling join syntax, which duckplyr lowers to DuckDB ASOF JOIN.

### Backend-preserving construction

`as_epi_archive_like(template, x, ...)` chooses the right archive factory from `template`'s subclass tag. Use it when an archive method returns a new archive and should preserve the input backend.

## Lifecycle and mutation semantics

- **Eager/data.table:** `archive_set_data()` materializes to a keyed `data.table`; mutation produces a new archive sharing nothing with the old.
- **Lazy/duckplyr:** `archive_set_data()` stores a new query plan. No table rewrite occurs until collection is forced by `archive_tbl()`, `archive_col()`, validation, printing, or similar eager operations.

This distinction is the reason the duck backend exists: composed pipelines can become one optimized DuckDB query rather than a chain of in-memory rewrites.

## DuckDB connection model

Duck archives created from R data frames are in-memory. `as_duckdb_epi_archive_from_duck()` may wrap an existing duckplyr/dbplyr relation, including one backed by a user-managed DuckDB connection, without changing accessor contracts. Persistence and connection ownership follow the wrapped relation; package code should treat `$duck` as an opaque lazy handle.

## Algorithm portability notes

Archive algorithms should prefer `archive_data()` pipelines and return through `archive_set_data()` / `as_epi_archive_like()`. For simple row restrictions on an archive object, prefer `archive_filter(x, ...)` so lazy backends keep the predicate in the query plan.

Useful patterns:

- Use `union_all()` rather than `bind_rows()` for lazy-dt compatibility; `bind_rows()` does not dispatch on `lazy_dt`.
- Preserve typed missing values with `~ .x[NA]`; plain `NA` can become logical and break type-strict set operations.
- For `epix_as_of()`, prefer `arrange(desc(version)) %>% distinct(across(nonversion_keys), .keep_all = TRUE)` over `slice_max()`: dtplyr can emit non-portable translations for `slice_max(order_by = ...)`.
- Printing should dispatch by backend. DT can print `$DT[]`; duck should print an eager `archive_tbl()` preview.
- For grouped `epix_slide(..., .all_versions = TRUE)`, chunking currently needs eager `archive_tbl()` data before constructing per-group archives.

### dtplyr gotchas

- `funion()` is type-strict; logical `NA`s introduced during mutation can break it.
- `as.data.table()` on a `dtplyr_step` ignores the `key` argument. The DT setter should collect first, then set the key explicitly with `setkeyv()`.

### Duck gotchas

- DuckDB does not preserve R factor/ordered columns. Code relying on factor semantics, especially `.drop = FALSE` grouping behavior, needs DT-specific tests or explicit coercion.
- Some R-specific vector classes may round-trip differently through DuckDB (e.g. `difftime` units). Tests should assert semantic equality where backend representation can differ.

## Testing strategy

Behavior-level archive tests should avoid backend internals and assert through accessors:

```r
archive_tbl(x)
archive_col(x, "version")
archive_colnames(x)
archive_nrow(x)
key_colnames(x)
```

DT implementation details such as `data.table::key(x$DT)`, by-reference mutation, or `$DT` class should be DT-specific tests and skipped in duck mode. Snapshot tests whose expected text includes backend-specific formatting should remain DT-only; duck should get semantic assertions instead of separate formatting snapshots.

`tests/testthat/helper-archive-backend.R` provides a backend-switching test mode. With:

```sh
EPIPROCESS_TEST_ARCHIVE_BACKEND=duck
```

unqualified test calls to `as_epi_archive()` construct duck archives. Default mode leaves `as_epi_archive()` unchanged. Use `test_dir()`, `test_local()`, or `test_check()` so helpers are loaded; bare `test_file()` does not load helpers automatically.

Common commands are in `Justfile`:

```sh
distrobox enter rocker -- just test-dt
distrobox enter rocker -- just test-duck
distrobox enter rocker -- just test-backends
```

The duck-switched suite uses `TESTTHAT_PARALLEL=false` because the current helper-shadowing approach and duck/testthat subprocess startup are not reliably parallel-safe.

## Known follow-ups

- autoplot() with duck backend:
  1. Fix archive_col usage in epix_as_of validation (small change, big win —
  eliminates ~3 full column pulls per snapshot).
  2. Cache archive_col_range results, or store version/time-value class on the
  archive object at construction time so validation is metadata-only.
  3. Rewrite the autoplot snapshot loop to a single query (bigger refactor, but
  turns linear-in-versions DuckDB overhead into constant).