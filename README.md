# FLSkill: Fisheries Prediction Skill Evaluation

A comprehensive R package for evaluating prediction skill in fisheries stock assessment models, with full compatibility with the FLR framework.

## Features

- **Standardized Metrics**: Implements widely-used skill assessment metrics including TSS (True Skill Statistic), AUC (Area Under Curve), and ROC analysis
- **FLR Integration**: Full support for FLQuants objects from the FLR package
- **Comprehensive Analysis**: Multiple diagnostic tools for trend analysis, variability comparison, and time series evaluation
- **S4 Methods**: Object-oriented design with S4 generics and methods for extensibility

## Installation

```r
# Install from GitHub (if available)
devtools::install_github("your-repo/FLSkill")

# Or install from local source
install.packages("path/to/FLSkill", repos = NULL, type = "source")
```

## Quick Start

### Basic Usage with Numeric Vectors

```r
library(FLSkill)

# Generate sample data
obs <- rlnorm(100, meanlog = log(1), sdlog = 0.3)
pred <- obs * exp(rnorm(100, sd = 0.1))

# Calculate skill scores
skill_result <- skillScore(obs, pred)
print(skill_result)

# Comprehensive summary
summary_result <- skillSummary(obs, pred)
print(summary_result)
```

### FLR Integration with FLQuants

```r
library(FLCore)
library(FLSkill)

# Create FLQuants objects
obs_data <- FLQuants(
  biomass = FLQuant(rlnorm(50, meanlog = log(1), sdlog = 0.3), 
                   dimnames = list(year = 2000:2049))
)

pred_data <- FLQuants(
  biomass = obs_data$biomass * exp(rnorm(50, sd = 0.1))
)

# All FLSkill functions work seamlessly with FLQuants
skill_result <- skillScore(obs_data$biomass, pred_data$biomass)
trend_result <- trend(obs_data$biomass, pred_data$biomass)
state_result <- state(obs_data$biomass, pred_data$biomass)
```

## Available Functions

### Core Skill Assessment
- `skillScore()`: Calculate prediction skill scores with optimal threshold tuning
- `skillSummary()`: Comprehensive performance metrics for fishery management procedures
- `TSS()`: True Skill Statistic calculation from confusion matrix elements
- `PN()`: Confusion matrix statistics (TP, TN, FP, FN)

### ROC Analysis
- `rocFn()`: ROC curve coordinates calculation
- `roc2()`: Comprehensive ROC statistics
- `rocFn2()`: Alternative ROC curve generator

### Diagnostic Tools
- `trend()`: Trend agreement metrics between time series
- `state()`: Stock status classification performance
- `variability()`: Variability characteristics comparison
- `compareTS()`: Time series similarity measures
- `ccfFn()`: Cross-correlation function for lag analysis
- `diagnostics()`: Comprehensive diagnostic evaluation

### Visualization
- `skillPlot()`: Diagnostic plots for management procedure evaluation
- `taylorDiagram()`: Taylor diagram for model comparison

## FLQuants Methods

All core functions have corresponding methods for FLQuants objects:

```r
# Methods available for FLQuants objects
skillScore(obs_FLQuants, pred_FLQuants)
skillSummary(obs_FLQuants, pred_FLQuants)
trend(obs_FLQuants, pred_FLQuants)
state(obs_FLQuants, pred_FLQuants)
variability(obs_FLQuants, pred_FLQuants)
compareTS(obs_FLQuants, pred_FLQuants)
ccfFn(obs_FLQuants, pred_FLQuants)
diagnostics(obs_FLQuants, pred_FLQuants)
TSS(TP_FLQuants, TN_FLQuants, FP_FLQuants, FN_FLQuants)
PN(obs_FLQuants, hat_FLQuants)
rocFn(labels_FLQuants, ind_FLQuants)
roc2(state_FLQuants, ind_FLQuants)
```

## Examples

See the `examples/` directory for detailed usage examples:
- `FLQuants_example.R`: Comprehensive example using FLQuants objects

## Testing

Run the test suite:

```r
library(testthat)
test_package("FLSkill")
```

## Dependencies

- R (>= 4.0.0)
- FLCore (>= 2.6.12)
- ggplot2
- caret
- pROC
- plyr
- reshape2
- MASS
- methods

## License

GPL-3

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
