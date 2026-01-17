# Tests for confidence intervals in skill metrics

test_that("skillScore returns CIs when requested", {
  set.seed(123)
  obs = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  pred = obs * exp(rnorm(100, sd = 0.2))
  
  # Without CI
  result_no_ci = skillScore(obs, pred, ci = FALSE)
  expect_false(any(grepl("CI", names(result_no_ci))))
  
  # With CI (using fewer bootstrap samples for speed)
  result_ci = skillScore(obs, pred, ci = TRUE, nBoot = 100, seed = 123)
  
  # Check that CI columns exist
  expect_true("AUC_CI_lower" %in% names(result_ci))
  expect_true("AUC_CI_upper" %in% names(result_ci))
  expect_true("TSS_CI_lower" %in% names(result_ci))
  expect_true("TSS_CI_upper" %in% names(result_ci))
  expect_true("TPR_CI_lower" %in% names(result_ci))
  expect_true("TPR_CI_upper" %in% names(result_ci))
  expect_true("FPR_CI_lower" %in% names(result_ci))
  expect_true("FPR_CI_upper" %in% names(result_ci))
  
  # Check that estimates match (without CI)
  expect_equal(result_no_ci$AUC, result_ci$AUC, tolerance = 1e-6)
  expect_equal(result_no_ci$TSS, result_ci$TSS, tolerance = 1e-6)
  
  # Check that CIs are reasonable (lower < estimate < upper)
  expect_true(result_ci$AUC_CI_lower < result_ci$AUC)
  expect_true(result_ci$AUC < result_ci$AUC_CI_upper)
  expect_true(result_ci$TSS_CI_lower < result_ci$TSS)
  expect_true(result_ci$TSS < result_ci$TSS_CI_upper)
  
  # Check that CIs are in valid ranges
  expect_true(result_ci$AUC_CI_lower >= 0 && result_ci$AUC_CI_lower <= 1)
  expect_true(result_ci$AUC_CI_upper >= 0 && result_ci$AUC_CI_upper <= 1)
  expect_true(result_ci$TSS_CI_lower >= -1 && result_ci$TSS_CI_lower <= 1)
  expect_true(result_ci$TSS_CI_upper >= -1 && result_ci$TSS_CI_upper <= 1)
})

test_that("skillSummary returns CIs when requested", {
  set.seed(123)
  obs = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  pred = obs * exp(rnorm(100, sd = 0.2))
  
  # Without CI
  result_no_ci = skillSummary(obs, pred, ci = FALSE)
  expect_false(any(grepl("CI", names(result_no_ci))))
  
  # With CI
  result_ci = skillSummary(obs, pred, ci = TRUE, nBoot = 100, seed = 123)
  
  # Check that CI columns exist
  expect_true("AUC_CI_lower" %in% names(result_ci))
  expect_true("AUC_CI_upper" %in% names(result_ci))
  expect_true("TSS_CI_lower" %in% names(result_ci))
  expect_true("TSS_CI_upper" %in% names(result_ci))
  
  # Check that estimates match
  expect_equal(result_no_ci$AUC, result_ci$AUC, tolerance = 1e-6)
  expect_equal(result_no_ci$TSS, result_ci$TSS, tolerance = 1e-6)
  
  # Check that CIs are reasonable
  expect_true(result_ci$AUC_CI_lower < result_ci$AUC)
  expect_true(result_ci$AUC < result_ci$AUC_CI_upper)
})

test_that("CI calculation handles edge cases", {
  set.seed(123)
  
  # Small sample size
  obs_small = rlnorm(10, meanlog = log(1), sdlog = 0.5)
  pred_small = obs_small * exp(rnorm(10, sd = 0.2))
  
  # Should handle small samples gracefully
  result = skillScore(obs_small, pred_small, ci = TRUE, nBoot = 50, seed = 123)
  expect_true(is.data.frame(result))
  # CIs might be NA for very small samples, which is acceptable
  
  # Perfect separation
  obs_perf = c(rep(0, 50), rep(1, 50))
  pred_perf = c(rnorm(50, mean = 0, sd = 0.5), rnorm(50, mean = 2, sd = 0.5))
  
  result_perf = skillScore(obs_perf, pred_perf, threshold = 0.5, ci = TRUE, nBoot = 100, seed = 123)
  expect_true(is.data.frame(result_perf))
  expect_true(result_perf$AUC > 0.9)  # Should be high for perfect separation
})

test_that("CI level parameter works", {
  set.seed(123)
  obs = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  pred = obs * exp(rnorm(100, sd = 0.2))
  
  # 90% CI
  result_90 = skillScore(obs, pred, ci = TRUE, ciLevel = 0.90, nBoot = 100, seed = 123)
  
  # 95% CI (default)
  result_95 = skillScore(obs, pred, ci = TRUE, ciLevel = 0.95, nBoot = 100, seed = 123)
  
  # 90% CI should be narrower than 95% CI
  ci_width_90 = result_90$AUC_CI_upper - result_90$AUC_CI_lower
  ci_width_95 = result_95$AUC_CI_upper - result_95$AUC_CI_lower
  
  expect_true(ci_width_90 < ci_width_95)
})

test_that("Seed parameter provides reproducibility", {
  set.seed(123)
  obs = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  pred = obs * exp(rnorm(100, sd = 0.2))
  
  # Same seed should give same results
  result1 = skillScore(obs, pred, ci = TRUE, nBoot = 100, seed = 456)
  result2 = skillScore(obs, pred, ci = TRUE, nBoot = 100, seed = 456)
  
  expect_equal(result1$AUC_CI_lower, result2$AUC_CI_lower, tolerance = 1e-6)
  expect_equal(result1$AUC_CI_upper, result2$AUC_CI_upper, tolerance = 1e-6)
  expect_equal(result1$TSS_CI_lower, result2$TSS_CI_lower, tolerance = 1e-6)
  expect_equal(result1$TSS_CI_upper, result2$TSS_CI_upper, tolerance = 1e-6)
})

test_that("CI calculation is consistent across different reference values", {
  set.seed(123)
  obs = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  pred = obs * exp(rnorm(100, sd = 0.2))
  
  # Different reference values
  result_ref1 = skillScore(obs, pred, reference = 0.8, ci = TRUE, nBoot = 100, seed = 123)
  result_ref2 = skillScore(obs, pred, reference = 1.0, ci = TRUE, nBoot = 100, seed = 123)
  result_ref3 = skillScore(obs, pred, reference = 1.2, ci = TRUE, nBoot = 100, seed = 123)
  
  # All should have valid CIs
  expect_true(all(!is.na(result_ref1[c("AUC_CI_lower", "AUC_CI_upper")])))
  expect_true(all(!is.na(result_ref2[c("AUC_CI_lower", "AUC_CI_upper")])))
  expect_true(all(!is.na(result_ref3[c("AUC_CI_lower", "AUC_CI_upper")])))
})
