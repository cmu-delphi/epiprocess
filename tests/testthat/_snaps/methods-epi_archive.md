# epix_as_of_latest works as expected

    `epix_as_of_current()` was deprecated in epiprocess 0.13.0.
    i Please use `epix_as_of_latest()` instead.
    i Future versions of `epix_as_of_current` may remove this deprecation, but change its implementation to require "current" to be sometime recent, i.e., require there to be a nontrivial recent update to the data.

# filter.epi_archive works as expected

    Code
      ea2 %>% filter(version <= as.Date("2020-06-02"))
    Condition <rlang_error>
      Error in `filter()`:
      i In argument: `version <= as.Date("2020-06-02")`.
      Caused by error:
      ! Using `version` in `filter.epi_archive` may produce unexpected results.
      > See if `epix_as_of` or `epix_slide` would work instead.
      > If not, see `?filter.epi_archive` details for how to proceed.

---

    Code
      ea2 %>% filter(time_value >= as.Date("2020-06-02"), cases >= 2L)
    Condition <rlang_error>
      Error in `filter()`:
      i In argument: `cases >= 2L`.
      Caused by error:
      ! Using `cases` in `filter.epi_archive` may produce unexpected results.
      > See `?filter.epi_archive` details for how to proceed.

---

    Code
      ea2p %>% filter(cases >= median(cases), .by = geo_value)
    Condition <rlang_error>
      Error in `filter()`:
      i In argument: `cases >= median(cases)`.
      i In group 1: `geo_value = "ca"`.
      Caused by error:
      ! Using `cases` in `filter.epi_archive` may produce unexpected results.
      > See `?filter.epi_archive` details for how to proceed.

