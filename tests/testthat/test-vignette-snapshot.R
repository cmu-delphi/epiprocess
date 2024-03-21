# Vignettes that use epi_archives or epi_dfs.
vignettes <- paste0(here::here("vignettes/"), c(
  "advanced.Rmd",
  "aggregation.Rmd",
  "archive.Rmd",
  "epiprocess.Rmd",
  "slide.Rmd"
))
for (input_file in vignettes) {
  test_that(paste0("snapshot vignette ", basename(input_file)), {
    skip("Skipping snapshot tests by default, as they are slow.")
    output_file <- sub("\\.Rmd$", ".html", input_file)
    withr::with_file(output_file, {
      devtools::build_rmd(input_file)
      expect_snapshot_file(output_file)
    })
  })
}
