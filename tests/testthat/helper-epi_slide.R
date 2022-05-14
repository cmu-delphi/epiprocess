## Create an epi. df and a function to test epi_slide with

edf = bind_rows(
  tibble(geo_value = "ak", time_value = as.Date("2020-01-01") + 1:200, value=1:200),
  tibble(geo_value = "al", time_value=as.Date("2020-01-01") + 1:5, value=-(1:5))
) %>%
  as_epi_df()

f = function(x, ...) tibble(value=mean(x$value), count=length(x$value))
