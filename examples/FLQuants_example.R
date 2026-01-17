# Example: Using FLSkill with FLQuants objects
# This example demonstrates how to use the FLSkill package with FLR FLQuants objects

library(FLCore)
library(FLSkill)
library(statcomp)

# Create sample FLQuants objects for demonstration
set.seed(123)

# Simulate observed biomass and catch data
years = 2000:2049
obsBiomass = FLQuant(rlnorm(50, meanlog = log(1), sdlog = 0.3), 
                       dimnames = list(year = years))
obsCatch = FLQuant(rlnorm(50, meanlog = log(0.5), sdlog = 0.2), 
                     dimnames = list(year = years))

# Create FLQuants object for observed data
obsData = FLQuants(
  biomass = obsBiomass,
  catch = obsCatch
)

# Simulate predicted data (with some error)
predBiomass = obsBiomass * exp(rnorm(50, sd = 0.1))
predCatch = obsCatch * exp(rnorm(50, sd = 0.1))

predData = FLQuants(
  biomass = predBiomass,
  catch = predCatch
)

# Example 1: Calculate skill scores
cat("=== Skill Score Analysis ===\n")
skillResult = skillScore(obsData$biomass, predData$biomass)
print(skillResult)

# Example 2: Calculate comprehensive skill summary
cat("\n=== Skill Summary ===\n")
summaryResult = skillSummary(obsData$biomass, predData$biomass)
print(summaryResult)

# Example 3: Trend analysis
cat("\n=== Trend Analysis ===\n")
trendResult = trend(obsData$biomass, predData$biomass)
print(trendResult)

# Example 4: Stock status classification
cat("\n=== Stock Status Classification ===\n")
stateResult = state(obsData$biomass, predData$biomass)
print(stateResult)

# Example 5: Variability comparison
cat("\n=== Variability Comparison ===\n")
varResult = variability(obsData$biomass, predData$biomass)
print(varResult)

# Example 6: Time series comparison
cat("\n=== Time Series Comparison ===\n")
tsResult = compareTS(obsData$biomass, predData$biomass)
print(tsResult)

# Example 7: Cross-correlation analysis
cat("\n=== Cross-Correlation Analysis ===\n")
ccfResult = ccfFn(obsData$biomass, predData$biomass)
print(ccfResult)

# Example 8: ROC analysis
cat("\n=== ROC Analysis ===\n")
# Create binary labels (overfished vs healthy)
labels = FLQuant(as.numeric(obsData$biomass) > 1, 
                 dimnames = list(year = years))
scores = FLQuant(as.numeric(predData$biomass), 
                 dimnames = list(year = years))

rocResult = rocFn(labels, scores)
print(head(rocResult))

# Example 9: Comprehensive diagnostics
cat("\n=== Comprehensive Diagnostics ===\n")
diagResult = diagnostics(obsData$biomass, predData$biomass)
print(diagResult)

# Example 10: TSS calculation with confusion matrix elements
cat("\n=== TSS Calculation ===\n")
# Create confusion matrix elements as FLQuants
tp = FLQuant(45, dimnames = list(year = 1))
tn = FLQuant(30, dimnames = list(year = 1))
fp = FLQuant(10, dimnames = list(year = 1))
fn = FLQuant(15, dimnames = list(year = 1))

tssResult = TSS(tp, tn, fp, fn)
print(tssResult)

# Example 11: Confusion matrix calculation
cat("\n=== Confusion Matrix ===\n")
# Create positive/negative indicators
obsSign = FLQuant(ifelse(as.numeric(obsData$biomass) > 1, 1, -1), 
                   dimnames = list(year = years))
predSign = FLQuant(ifelse(as.numeric(predData$biomass) > 1, 1, -1), 
                    dimnames = list(year = years))

confusionResult = PN(obsSign, predSign)
print(confusionResult)

cat("\n=== Example completed successfully! ===\n")
cat("All FLSkill functions work seamlessly with FLQuants objects.\n") 