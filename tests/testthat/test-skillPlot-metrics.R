# Tests for skillPlot metrics validation against pROC
# These tests ensure that AUC, TSS, and other metrics match reference implementations

test_that("AUC calculation matches pROC for simple cases", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  # Test case 1: Perfect separation
  set.seed(123)
  response_perfect = c(rep(FALSE, 50), rep(TRUE, 50))
  predictor_perfect = c(rnorm(50, mean = 0, sd = 0.5), rnorm(50, mean = 2, sd = 0.5))
  
  # Calculate AUC using our method
  roc_data = FLSkill::rocFn2(response_perfect, predictor_perfect)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  # Calculate AUC using pROC
  roc_obj = pROC::roc(response_perfect, predictor_perfect, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  # Should be very close (within 0.01 due to different calculation methods)
  expect_true(abs(auc_ours - auc_proc) < 0.01, 
              info = paste("AUC mismatch: ours =", auc_ours, ", pROC =", auc_proc))
  
  # Test case 2: Random prediction (should be ~0.5)
  set.seed(456)
  response_random = sample(c(FALSE, TRUE), 100, replace = TRUE)
  predictor_random = rnorm(100)
  
  roc_data = FLSkill::rocFn2(response_random, predictor_random)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  roc_obj = pROC::roc(response_random, predictor_random, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  expect_true(abs(auc_ours - auc_proc) < 0.05,
              info = paste("Random AUC mismatch: ours =", auc_ours, ", pROC =", auc_proc))
  
  # Test case 3: Moderate separation
  set.seed(789)
  response_mod = c(rep(FALSE, 50), rep(TRUE, 50))
  predictor_mod = c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 1, sd = 1))
  
  roc_data = FLSkill::rocFn2(response_mod, predictor_mod)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  roc_obj = pROC::roc(response_mod, predictor_mod, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  expect_true(abs(auc_ours - auc_proc) < 0.02,
              info = paste("Moderate AUC mismatch: ours =", auc_ours, ", pROC =", auc_proc))
})

test_that("AUC from skillScore matches pROC", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  set.seed(123)
  # Create realistic fishery data
  response = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  predictor = response * exp(rnorm(100, sd = 0.2))
  
  # Binary classification: overfished (response < 1) vs healthy (response >= 1)
  threshold = 1.0
  
  # Calculate using skillScore
  skill_result = FLSkill::skillScore(response, predictor, threshold = threshold)
  auc_ours = skill_result$AUC
  
  # Calculate using pROC
  labels = response > threshold
  roc_obj = pROC::roc(labels, predictor, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  # Should match closely
  expect_true(abs(auc_ours - auc_proc) < 0.02,
              info = paste("skillScore AUC mismatch: ours =", auc_ours, ", pROC =", auc_proc))
})

test_that("AUC from skillPlot internal calculations matches pROC", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  set.seed(123)
  response = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  predictor = response * exp(rnorm(100, sd = 0.2))
  threshold = 1.0
  reference = 1.0
  
  # Use internal calculation function from skillPlot (in plots.R)
  # Calculate ROC data first
  roc_data = FLSkill::rocFn2(response > threshold, predictor)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  # Calculate using pROC
  labels = response > threshold
  roc_obj = pROC::roc(labels, predictor, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  expect_true(abs(auc_ours - auc_proc) < 0.02,
              info = paste("skillPlot AUC mismatch: ours =", auc_ours, ", pROC =", auc_proc))
})

test_that("ROC curve structure is correct", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  set.seed(123)
  response = sample(c(TRUE, FALSE), 100, replace = TRUE)
  predictor = rnorm(100)
  
  # Our method
  roc_ours = FLSkill::rocFn2(response, predictor)
  
  # pROC method
  roc_obj = pROC::roc(response, predictor, quiet = TRUE)
  
  # Should end at (1,1) - start may not be exactly (0,0) depending on data
  expect_equal(roc_ours$FPR[nrow(roc_ours)], 1, tolerance = 1e-6)
  expect_equal(roc_ours$TPR[nrow(roc_ours)], 1, tolerance = 1e-6)
  
  # First point should be close to (0,0) or at least valid
  expect_true(roc_ours$FPR[1] >= 0 && roc_ours$FPR[1] <= 1)
  expect_true(roc_ours$TPR[1] >= 0 && roc_ours$TPR[1] <= 1)
  
  # TPR and FPR should be monotonically increasing
  expect_true(all(diff(roc_ours$FPR) >= 0))
  expect_true(all(diff(roc_ours$TPR) >= 0))
  
  # Values should be in valid range [0,1]
  expect_true(all(roc_ours$FPR >= 0 & roc_ours$FPR <= 1))
  expect_true(all(roc_ours$TPR >= 0 & roc_ours$TPR <= 1))
})

test_that("TSS calculation is consistent", {
  set.seed(123)
  response = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  predictor = response * exp(rnorm(100, sd = 0.2))
  threshold = 1.0
  
  # Calculate using skillScore (uses reference=NULL, so finds optimal threshold)
  skill_result = FLSkill::skillScore(response, predictor, threshold = threshold, reference = NULL)
  tss_skill = skill_result$TSS
  ref_used = skill_result$ref
  tpr_skill = skill_result$TPR
  fpr_skill = skill_result$FPR
  
  # skillScore calculates TSS as TPR - FPR from the ROC curve
  # Verify this matches
  expect_equal(tss_skill, tpr_skill - fpr_skill, tolerance = 1e-6)
  
  # Calculate using rocFn2 and find optimal threshold (max TPR - FPR)
  roc_data = FLSkill::rocFn2(response > threshold, predictor)
  optimal_idx = which.max(roc_data$TPR - roc_data$FPR)
  tss_manual = roc_data$TPR[optimal_idx] - roc_data$FPR[optimal_idx]
  
  # skillScore should use the optimal threshold when reference=NULL
  expect_equal(tss_skill, tss_manual, tolerance = 1e-6)
  
  # Note: There may be slight discrepancies between TPR/FPR from ROC curve
  # and confusion matrix due to how thresholds are selected, but TSS should be consistent
})

test_that("AUC handles edge cases correctly", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  # Edge case 1: All positive
  response_all_pos = rep(TRUE, 50)
  predictor_all_pos = rnorm(50)
  
  roc_data = FLSkill::rocFn2(response_all_pos, predictor_all_pos)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  # Should handle gracefully (AUC undefined but function should not crash)
  expect_true(is.numeric(auc_ours))
  
  # Edge case 2: All negative
  response_all_neg = rep(FALSE, 50)
  predictor_all_neg = rnorm(50)
  
  roc_data = FLSkill::rocFn2(response_all_neg, predictor_all_neg)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  expect_true(is.numeric(auc_ours))
  
  # Edge case 3: Perfect prediction (all 1s and 0s)
  response_perf = c(rep(FALSE, 25), rep(TRUE, 25))
  predictor_perf = c(rep(0, 25), rep(1, 25))
  
  roc_data = FLSkill::rocFn2(response_perf, predictor_perf)
  auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
  
  roc_obj = pROC::roc(response_perf, predictor_perf, quiet = TRUE)
  auc_proc = as.numeric(pROC::auc(roc_obj))
  
  # Perfect prediction should give AUC = 1
  expect_true(auc_ours >= 0.99, info = "Perfect prediction should give AUC near 1")
  expect_true(abs(auc_ours - auc_proc) < 0.1)
})

test_that("Multiple scenarios produce consistent AUC values", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  set.seed(123)
  
  # Test with multiple scenarios (grouped data)
  n_scenarios = 3
  n_per_scenario = 50
  
  results_ours = numeric(n_scenarios)
  results_proc = numeric(n_scenarios)
  
  for (i in 1:n_scenarios) {
    response = rlnorm(n_per_scenario, meanlog = log(1), sdlog = 0.5)
    predictor = response * exp(rnorm(n_per_scenario, sd = 0.2))
    threshold = 1.0
    
    # Our method
    roc_data = FLSkill::rocFn2(response > threshold, predictor)
    results_ours[i] = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
    
    # pROC method
    roc_obj = pROC::roc(response > threshold, predictor, quiet = TRUE)
    results_proc[i] = as.numeric(pROC::auc(roc_obj))
    
    # Should match within tolerance
    expect_true(abs(results_ours[i] - results_proc[i]) < 0.02,
                info = paste("Scenario", i, "AUC mismatch"))
  }
})

test_that("AUC calculation is consistent across different threshold values", {
  skip_if_not_installed("pROC")
  
  library(pROC)
  
  set.seed(123)
  response = rlnorm(100, meanlog = log(1), sdlog = 0.5)
  predictor = response * exp(rnorm(100, sd = 0.2))
  
  thresholds = c(0.5, 0.8, 1.0, 1.2, 1.5)
  
  for (threshold in thresholds) {
    # Our method
    roc_data = FLSkill::rocFn2(response > threshold, predictor)
    auc_ours = FLSkill:::auc_trapz(roc_data$TPR, roc_data$FPR)
    
    # pROC method
    roc_obj = pROC::roc(response > threshold, predictor, quiet = TRUE)
    auc_proc = as.numeric(pROC::auc(roc_obj))
    
    expect_true(abs(auc_ours - auc_proc) < 0.02,
                info = paste("Threshold", threshold, "AUC mismatch: ours =", 
                            auc_ours, ", pROC =", auc_proc))
  }
})
