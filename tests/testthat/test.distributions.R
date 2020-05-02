context("Distribution Parameters")
library(bistr)
library(pracma)

test_that("lnorm_sdlog gives correct answers", {
  gini_1 = 0.5
  expect_equal(gini_1,erf(lnorm_sdlog(gini_1)/2))
  gini_2 = 0.25
  expect_equal(gini_2,erf(lnorm_sdlog(gini_2)/2))
  gini_3 = 0.1
  expect_equal(gini_3,erf(lnorm_sdlog(gini_3)/2))
})

test_that("pareto_shape gives correct answers", {
  gini_1 = 0.5
  expect_equal(gini_1, 1 / (2 * pareto_shape(gini_1) - 1))
  gini_2 = 0.25
  expect_equal(gini_2, 1 / (2 * pareto_shape(gini_2) - 1))
  gini_3 = 0.1
  expect_equal(gini_3, 1 / (2 * pareto_shape(gini_3) - 1))
})

test_that("pareto_location gives correct answers", {
  mean_1 = 1000
  gini_1 = 0.5
  shape_1 = pareto_shape(gini_1)
  expect_equal(mean_1,
      (shape_1 * pareto_location(mean_1, gini_1)) / (shape_1 - 1)
  )
  gini_2 = 0.25
  shape_2 = pareto_shape(gini_2)
  expect_equal(mean_1,
      (shape_2 * pareto_location(mean_1, gini_2)) / (shape_2 - 1)
  )
  gini_3 = 0.1
  shape_3 = pareto_shape(gini_3)
  expect_equal(mean_1,
      (shape_3 * pareto_location(mean_1, gini_3)) / (shape_3 - 1)
  )
  mean_4 = 10000
  gini_4 = 0.1
  shape_4 = pareto_shape(gini_4)
  expect_equal(mean_4,
               (shape_4 * pareto_location(mean_4, gini_4)) / (shape_4 - 1)
  )
})
