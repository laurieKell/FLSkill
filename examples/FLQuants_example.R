# Example: Using FLSkill with FLQuants objects
# This example demonstrates how to use the FLSkill package with FLR FLQuants objects

library(FLCore)
library(FLSkill)

# Create sample FLQuants objects for demonstration
set.seed(123)

# Simulate observed biomass and catch data
years=2000:2049
obs_biomass=FLQuant(rlnorm(50, meanlog = log(1), sdlog = 0.3), 
                       dimnames = list(year = years))
obs_catch=FLQuant(rlnorm(50, meanlog = log(0.5), sdlog = 0.2), 
                     dimnames = list(year = years))

# Create FLQuants object for observed data
obs_data=FLQuants(
  biomass = obs_biomass,
  catch = obs_catch
)

# Simulate predicted data (with some error)
pred_biomass=obs_biomass * exp(rnorm(50, sd = 0.1))
pred_catch=obs_catch * exp(rnorm(50, sd = 0.1))

pred_data=FLQuants(
  biomass = pred_biomass,
  catch = pred_catch
)

# Example 1: Calculate skill scores
cat("=== Skill Score Analysis ===\n")
skill_result=skillScore(obs_data$biomass, pred_data$biomass)
print(skill_result)

# Example 2: Calculate comprehensive skill summary
cat("\n=== Skill Summary ===\n")
summary_result=skillSummary(obs_data$biomass, pred_data$biomass)
print(summary_result)

# Example 3: Trend analysis
cat("\n=== Trend Analysis ===\n")
trend_result=trend(obs_data$biomass, pred_data$biomass)
print(trend_result)

# Example 4: Stock status classification
cat("\n=== Stock Status Classification ===\n")
state_result=state(obs_data$biomass, pred_data$biomass)
print(state_result)

# Example 5: Variability comparison
cat("\n=== Variability Comparison ===\n")
var_result=variability(obs_data$biomass, pred_data$biomass)
print(var_result)

# Example 6: Time series comparison
cat("\n=== Time Series Comparison ===\n")
ts_result=compareTS(obs_data$biomass, pred_data$biomass)
print(ts_result)

# Example 7: Cross-correlation analysis
cat("\n=== Cross-Correlation Analysis ===\n")
ccf_result=ccfFn(obs_data$biomass, pred_data$biomass)
print(ccf_result)

# Example 8: ROC analysis
cat("\n=== ROC Analysis ===\n")
# Create binary labels (overfished vs healthy)
labels=FLQuant(as.numeric(obs_data$biomass) > 1, 
                 dimnames = list(year = years))
scores=FLQuant(as.numeric(pred_data$biomass), 
                 dimnames = list(year = years))

roc_result=rocFn(labels, scores)
print(head(roc_result))

# Example 9: Comprehensive diagnostics
cat("\n=== Comprehensive Diagnostics ===\n")
diag_result=diagnostics(obs_data$biomass, pred_data$biomass)
print(diag_result)

# Example 10: TSS calculation with confusion matrix elements
cat("\n=== TSS Calculation ===\n")
# Create confusion matrix elements as FLQuants
TP=FLQuant(45, dimnames = list(year = 1))
TN=FLQuant(30, dimnames = list(year = 1))
FP=FLQuant(10, dimnames = list(year = 1))
FN=FLQuant(15, dimnames = list(year = 1))

tss_result=TSS(TP, TN, FP, FN)
print(tss_result)

# Example 11: Confusion matrix calculation
cat("\n=== Confusion Matrix ===\n")
# Create positive/negative indicators
obs_sign=FLQuant(ifelse(as.numeric(obs_data$biomass) > 1, 1, -1), 
                   dimnames = list(year = years))
pred_sign=FLQuant(ifelse(as.numeric(pred_data$biomass) > 1, 1, -1), 
                    dimnames = list(year = years))

confusion_result=PN(obs_sign, pred_sign)
print(confusion_result)

cat("\n=== Example completed successfully! ===\n")
cat("All FLSkill functions work seamlessly with FLQuants objects.\n") 