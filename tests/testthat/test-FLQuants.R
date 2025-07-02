test_that("FLQuants methods work correctly", {
  # Skip if FLCore is not available
  skip_if_not_installed("FLCore")
  
  library(FLCore)
  
  # Create sample FLQuants objects
  set.seed(123)
  obs_data=FLQuants(
    biomass = FLQuant(rlnorm(50, meanlog = log(1), sdlog = 0.3), 
                     dimnames = list(year = 2000:2049)),
    catch = FLQuant(rlnorm(50, meanlog = log(0.5), sdlog = 0.2), 
                   dimnames = list(year = 2000:2049))
  )
  
  pred_data=FLQuants(
    biomass = obs_data$biomass * exp(rnorm(50, sd = 0.1)),
    catch = obs_data$catch * exp(rnorm(50, sd = 0.1))
  )
  
  # Test TSS method
  test_that("TSS works with FLQuants", {
    # Create confusion matrix elements as FLQuants
    TP=FLQuant(45, dimnames = list(year = 1))
    TN=FLQuant(30, dimnames = list(year = 1))
    FP=FLQuant(10, dimnames = list(year = 1))
    FN=FLQuant(15, dimnames = list(year = 1))
    
    result=TSS(TP, TN, FP, FN)
    expect_true(is(result, "FLQuants"))
    expect_equal(as.numeric(result[[1]]), 0.5) # Expected TSS value
  })
  
  # Test PN method
  test_that("PN works with FLQuants", {
    # Create positive/negative indicators
    obs_sign=FLQuant(ifelse(as.numeric(obs_data$biomass) > 1, 1, -1), 
                       dimnames = list(year = 2000:2049))
    pred_sign=FLQuant(ifelse(as.numeric(pred_data$biomass) > 1, 1, -1), 
                        dimnames = list(year = 2000:2049))
    
    result=PN(obs_sign, pred_sign)
    expect_true(is.data.frame(result))
    expect_true(all(c("TP", "TN", "FP", "FN") %in% names(result)))
  })
  
  # Test rocFn method
  test_that("rocFn works with FLQuants", {
    labels=FLQuant(as.numeric(obs_data$biomass) > 1, 
                     dimnames = list(year = 2000:2049))
    scores=FLQuant(as.numeric(pred_data$biomass), 
                     dimnames = list(year = 2000:2049))
    
    result=rocFn(labels, scores)
    expect_true(is.data.frame(result))
    expect_true(all(c("TPR", "FPR", "labels", "reference") %in% names(result)))
  })
  
  # Test roc2 method
  test_that("roc2 works with FLQuants", {
    result=roc2(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("state", "label", "ind", "TPR", "FPR", "TP", "TN", "FP", "FN", "TSS") %in% names(result)))
  })
  
  # Test skillScore method
  test_that("skillScore works with FLQuants", {
    result=skillScore(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("AUC", "TSS", "ref", "TPR", "FPR", "TP", "TN", "FP", "FN") %in% names(result)))
  })
  
  # Test skillSummary method
  test_that("skillSummary works with FLQuants", {
    result=skillSummary(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("AUC", "TSS", "ref", "TPR", "FPR") %in% names(result)))
  })
  
  # Test trend method
  test_that("trend works with FLQuants", {
    result=trend(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("pearson", "spearman", "direction") %in% names(result)))
  })
  
  # Test state method
  test_that("state works with FLQuants", {
    result=state(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("accuracy", "precision", "recall") %in% names(result)))
  })
  
  # Test variability method
  test_that("variability works with FLQuants", {
    result=variability(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("sd", "iqr", "cv") %in% names(result)))
  })
  
  # Test compareTS method
  test_that("compareTS works with FLQuants", {
    result=compareTS(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("rmse", "correlation", "sd") %in% names(result)))
  })
  
  # Test ccfFn method
  test_that("ccfFn works with FLQuants", {
    result=ccfFn(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("lag", "acf") %in% names(result)))
  })
  
  # Test diagnostics method
  test_that("diagnostics works with FLQuants", {
    result=diagnostics(obs_data$biomass, pred_data$biomass)
    expect_true(is.data.frame(result))
    expect_true(all(c("trend", "status", "sd.pred", "sd.obs", "variability", "auc", "tss", "fpr", "tpr", "entropy") %in% names(result)))
  })
}) 