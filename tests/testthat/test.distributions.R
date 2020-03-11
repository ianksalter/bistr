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
  # gini_1 = 0.5
  # expect_equal(gini_1, 1/(pareto_shape(gini_1) - 1))
  # gini_2 = 0.25
  # expect_equal(gini_2, 1/(pareto_shape(gini_2) - 1))
  # gini_3 = 0.1
  # expect_equal(gini_3, 1/(pareto_shape(gini_3) - 1))
})

test_that("pareto_location gives correct answers", {
  # mean_1 = 1000
  # gini_1 = 0.5
  # expect_equal(gini_1,(pareto_shape(gini_1)*pareto_location(mean,gini_1))/
  #                (pareto_shape(gini_1) - 1)
  # )
  # gini_2 = 0.25
  # expect_equal(gini_2,(pareto_shape(gini_2)*pareto_location(mean,gini_2))/
  #                (pareto_shape(gini_2) - 1)
  # )
  # gini_3 = 0.1
  # expect_equal(gini_3,(pareto_shape(gini_3)*pareto_location(mean,gini_3))/
  #                (pareto_shape(gini_3) - 1)
  # )
})
