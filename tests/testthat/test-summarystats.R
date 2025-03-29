library(testthat)
library(sumfunc)
test_that("descriptivestats works with single grouping variable", {
  expected_result <- data.frame(
    cyl = c(4, 6, 8),
    avg = c(26.7, 19.7, 15.1)
  )
  result <- summarystats(mtcars, rlang::exprs(cyl), mpg)

  expect_true(all.equal(result$avg, expected_result$avg, tolerance = 1e-2))
})
