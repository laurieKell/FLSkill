test_that("trend() calculates correlations correctly", {
  obs <- cumsum(rnorm(20))
  pred <- obs + rnorm(20, sd=0.5)
  result <- trend(obs, pred)
  expect_true(between(result$pearson, -1, 1))
  expect_true(between(result$direction, 0, 1))
})

test_that("skillScore() returns valid AUC", {
  obs <- runif(100, 0.5, 1.5)
  pred <- obs * exp(rnorm(100, sd=0.2))
  result <- skillScore(obs > 1, pred)
  expect_true(between(result$AUC, 0, 1))
})
