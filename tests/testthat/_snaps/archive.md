# new_epi_archive correctly detects and warns about compactification

    Code
      res <- dumb_ex %>% as_epi_archive()
    Condition
      Warning:
      Found rows that appear redundant based on last (version of each) observation carried forward; these rows have been removed to 'compactify' and save space:
      Key: <geo_value, time_value, version>
         geo_value time_value value    version
            <char>     <Date> <num>     <Date>
      1:        ca 2020-01-01     1 2020-01-02
      Built-in `epi_archive` functionality should be unaffected, but results may change if you work directly with its fields (such as `DT`). See `?as_epi_archive` for details. To silence this warning but keep compactification, you can pass `compactify=TRUE` when constructing the archive.

