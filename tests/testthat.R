library(testthat)
library(epiprocess)

stopifnot(packageVersion("testthat") >= "3.1.5")

test_check("epiprocess")
