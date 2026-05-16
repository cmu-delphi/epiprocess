# epiprocess: Domain Context

## Glossary

### epi_archive
A **bitemporal** data store: for every `(geo_value, other_keys..., time_value)` (the "epikey-time") there may be many rows, one per `version`. Tracks the full revision history of public-health data as it gets republished over time.

The unique key (ukey) is `c("geo_value", other_keys..., "time_value", "version")`. The non-version subset is the **epikey-time**.

### Backend abstraction
The storage layer behind `epi_archive` is abstracted so it can swap between:
- **data.table** — original implementation; in-memory, mutate-by-reference, keyed indexes. Subclass `epi_archive_dt`.
- **duckplyr** — DuckDB-backed lazy frame, for performance/scale. Subclass `epi_archive_duck`.

tsibble was considered and rejected: it requires one row per (key, index), which contradicts the bitemporal model. A pure dplyr/tibble backend may be added later but isn't a near-term goal.

## Backend abstraction design

The abstraction is at the **dplyr verb level**. Backends produce a handle that dplyr verbs operate on; algorithms write pipelines in pure dplyr. Custom verbs (`archive_distinct`, `archive_mutate`, ...) were rejected as reinventing dplyr with worse coverage.

### Per-archive dispatch

Each archive carries its backend tag as an S3 subclass: `c("epi_archive_dt", "epi_archive")` or `c("epi_archive_duck", "epi_archive")`. Accessors are S3 generics with one method per backend. This supports mixing backends in one session (e.g. small in-memory test fixtures alongside a large DuckDB-backed archive).

### Accessor contract

The abstraction is **dplyr verbs + a small set of domain primitives**. The dplyr verbs cover ~95% of what `epi_archive` algorithms need; the domain primitives are operations that dplyr can't express cleanly and that each backend implements differently.

**Core accessors:**
- `archive_data(x)` — returns the backend-native handle that dplyr verbs operate on. For the data.table backend: `dtplyr::lazy_dt(x$DT)`. For duckplyr: the lazy duckplyr frame. Algorithms write `archive_data(x) %>% distinct(...) %>% mutate(...)` etc.
- `archive_set_data(x, data)` — accepts any tabular (`lazy_dt`, tibble, `data.table`, duckplyr handle), normalizes to the backend's storage. **Eager backends** (data.table) materialize and re-key on each call; **lazy backends** (duckplyr) just store the new lazy reference and let the engine optimize across stages. Does not validate; use `validate_epi_archive` for that.
- `archive_col(x, col)` — returns a bare vector. On lazy backends this forces materialization of that column. Hot sites that aggregate (`max`, `range`, ...) can be migrated to a future `archive_summarize(x, expr)` push-down accessor if profiling demands it.
- `archive_columns_as_list(x)` — shallow list of column references; used by tidyeval machinery (`epix_detailed_restricted_mutate`) that needs pointer-equality between input and output columns.

**Domain primitives** (operations dplyr can't express cleanly across all backends):
- `archive_locf_join(left, right, by)` — rolling join with last-version-carried-forward semantics; last `by` column is the LOCF axis. Backend implementations: data.table's `[i, , on=by, roll=TRUE, nomatch=NA]`; DuckDB's `ASOF JOIN`.

### Lifecycle / mutation semantics

- **Eager (data.table)**: `archive_set_data` materializes to a keyed `data.table`. Mutation produces a new archive sharing nothing with the old.
- **Lazy (duckplyr)**: `archive_set_data` stores the new query plan. No table rewrite until something forces collection (`archive_col`, `validate_epi_archive`, or an explicit `archive_collect`). This is the whole reason duckplyr is interesting — pipelines compose into one optimized engine query, not N table rewrites.

### Connection model (duckdb)

In-memory only: each archive holds its own duckdb in-memory handle, opened transparently by duckplyr. No persistence, no shared connections. File-backed (`as_duckdb_epi_archive(data, path=...)`) is a small additive change if/when needed; would not change the accessor methods.

### Construction

Two factory functions:
- `as_epi_archive(data, ...)` — data.table-backed (the original; unchanged behavior).
- `as_duckdb_epi_archive(data, ...)` — duckdb-backed.

Backend choice as a constructor argument (`as_epi_archive(data, backend = "duckdb")`) was rejected: separate factories make the return type, lifecycle, and perf characteristics obvious at call sites. A future `as_epi_archive_from_duck(duck_table, ...)` would let users wrap an existing remote table without round-tripping through an R data.frame.

### The `$DT` public field

Stays as-is on the data.table backend; documented as a power-user escape hatch into raw data.table. The duck backend exposes a different field (`$duck`) for its escape hatch. There is intentionally no uniform `$data` field — eager vs lazy semantics differ enough that a single name would mislead.

### dtplyr coverage gotchas

Notes from migrating algorithms; future migrations should expect:

- **`bind_rows` doesn't dispatch on `lazy_dt`.** Use `union_all` (which is an S3 generic and dispatches via dtplyr to `data.table::funion`).
- **`funion` is type-strict.** Logical `NA`s introduced via `mutate(across(..., ~ NA))` break the set-op when the target columns are typed. Use `~ .x[NA]` to preserve the source column's type.
- **`as.data.table()` on a `dtplyr_step` ignores the `key` argument.** The DT setter collects first, then sets the key explicitly via `setkeyv` if it differs from `key_colnames(x)`.

## Current task: adding the duckplyr backend

Sequence:
1. **Convert accessors to S3 generics**, with the existing implementations becoming `.epi_archive_dt` methods. Pure mechanical refactor, no behavior change.
2. **Tag existing archives with the `epi_archive_dt` subclass** in `new_epi_archive` so dispatch finds the right methods.
3. **Add the duck backend**: `duckplyr` to Suggests; `as_duckdb_epi_archive()` constructor; `.epi_archive_duck` methods for every accessor.
4. **Parameterize the refactor-readiness test suite** to run against both backends, validating that public API behavior is identical.

## Refactor status

### Accessor layer (`R/archive_accessors.R`)
All accessors are S3 generics dispatching on the backend subclass tag
(`epi_archive_dt`, `epi_archive_duck`):

- `archive_data(x)` — backend-native dplyr handle. DT: `dtplyr::lazy_dt(x$DT)`. Duck: `x$duck`.
- `archive_set_data(x, data)` — replaces storage; normalizes any tabular input. DT materializes + re-keys; duck stores lazy ref.
- `archive_col(x, col)` — bare vector. Forces materialization on duck (one column).
- `archive_tbl(x)` — eager tibble. **Load-bearing**: used wherever downstream code needs a real data frame (compactify, `vec_split`, `vctrs::vec_split`, `validate_signal_format`, slide chunking, tidyeval requiring `.env$`). Not retiring.
- `archive_colmask(x)` — 0-row tibble with the archive's columns. For tidyselect data masks that only need schema (`eval_select`, `eval_pure_select_names_from_dots`).
- `archive_colnames`, `archive_nrow`, `archive_ncol`, `archive_any_duplicated_key`, `archive_deep_copy`, `archive_filter_rows`, `archive_col_is_factor`, `archive_columns_as_list` — all duck-implemented.
- `archive_locf_join(left, right, by)` — domain primitive with backend dispatch. DT/default impl uses data.table roll-join; duck impl uses dplyr rolling join syntax, which duckplyr lowers to DuckDB ASOF JOIN.

### Backend-preserving construction
`as_epi_archive_like(template, x, ...)` (in `archive_duck.R`) is an S3 generic that picks the right factory based on `template`'s subclass tag. Used by `filter.epi_archive` and `epix_merge` so the output archive's backend matches the input's.

### Duck backend
- Constructor: `as_duckdb_epi_archive(data, ...)` builds via `as_epi_archive` then swaps storage via `as_duckdb_archive` (short-circuits if input is already duck).
- Storage field: `$duck` (a `duckplyr_df` lazy frame). In-memory only; each archive owns its handle.
- Caveat: `archive_col_is_factor` always returns FALSE on duck — duckdb has no factor type, and round-tripping drops factors to character.

### Algorithms migrated
- `epix_fill_through_version` — `archive_data` + `union_all` + `archive_set_data`.
- `epix_merge` — `full_join` on key cols + `archive_locf_join` chain. ~190 → ~95 lines.
- `epix_detailed_restricted_mutate` — `archive_columns_as_list` + `archive_set_data`.
- `epix_slide.grouped_epi_archive` `all_versions=TRUE` path — `archive_tbl` + per-chunk `archive_set_data`.
- `epix_as_of` — `archive_data %>% filter %>% arrange(desc(version)) %>% distinct(across(nonversion_keys), .keep_all=TRUE) %>% arrange(nonversion_keys) %>% collect`. Replaces the data.table-only `unique(by=, fromLast=TRUE)`. Caveat: `slice_max(order_by=)` is not dtplyr-portable (translation emits unqualified `desc()`), hence the arrange+distinct form.

### Read-side sweep
All non-test `R/` code paths go through accessors. Printing now dispatches by backend: DT prints `$DT[]`; duck prints an eager `archive_tbl()` preview.

### Tests
- `tests/testthat/test-epi_archive-refactor-readiness.R` parameterized over both backends. Duck tests skip when duckplyr isn't installed. Includes backend-preservation assertions for `filter`, `epix_merge`, `epix_truncate_versions_after`, and grouped `epix_slide(.all_versions = TRUE)`, plus coverage for `epix_fill_through_version`.
- `tests/testthat/helper-archive-backend.R` is a POC for broader backend switching. `EPIPROCESS_TEST_ARCHIVE_BACKEND=duck` shadows unqualified `as_epi_archive()` calls in tests so they construct duck archives; default `dt` leaves the exported constructor alone. Use `test_dir()`/`test_check()` rather than bare `test_file()` so helpers are loaded.
- Full local test suite passing after the latest accessor/duck updates.

### Algorithms not yet exercised on the duck backend
The parameterized refactor-readiness suite covers `key_colnames`, `clone`, `epix_as_of`, `epix_merge`, `filter.epi_archive`, `epix_slide.grouped_epi_archive` (`.all_versions = TRUE`), `epix_truncate_versions_after`, and `epix_fill_through_version`. Likely-works-but-unverified on duck:
- `revision_analysis`
- `epix_pivot_wider`

Natural follow-up: extend the parameterized suite to cover these.

### Lower-priority cleanups (deferred)
- **Roxygen on internal accessors** — `archive_accessors.R` has full `@param`/`@return` blocks on 1-line wrappers. File-level comment already explains purpose.
- **`archive_any_duplicated_key()` contract mismatch** — DT returns the index of the first duplicated key (`anyDuplicated` semantics); duck currently returns a positive duplicate-group count. Existing validation only needs zero/nonzero, but the accessor docs and implementations should be reconciled before broader use.
- **Tests using `$DT`** (~89 refs in `tests/testthat/`) — to be migrated when the parameterized suite is broadened.
- **Roxygen examples that use `$DT`** (~12 refs) — public field, intentionally left.
