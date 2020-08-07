context("testtest")
library(fars)


test_that("data is returned", {
  data <- read.csv(system.file("extdata", "accident_2013.csv.bz2",
                               package = "fars"))
  expect_that(data, is_a("data.frame"))
})

