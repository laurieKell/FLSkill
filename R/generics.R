#' @title Calculate True Skill Statistic (TSS)
#'
#' @description Calculates the True Skill Statistic (TSS) from confusion matrix elements
#'
#' @param TP Number of True Positives
#' @param TN Number of True Negatives
#' @param FP Number of False Positives
#' @param FN Number of False Negatives
#'
#' @return Numeric value of TSS (sensitivity + specificity - 1)
#'
#' @details
#' TSS ranges from -1 to +1, where +1 indicates perfect agreement and values of zero 
#' or less indicate a performance no better than random
#'
#' @examples
#' \dontrun{
#' tss_value=TSS(TP=10, TN=8, FP=2, FN=3)
#' }
#'
#' @export
setGeneric("TSS", function(TP, TN, FP, FN) standardGeneric("TSS"))


#' @title Confusion Matrix Statistics
#'
#' @description Calculates True Positive (TP), True Negative (TN), False Positive (FP), 
#' and False Negative (FN) counts from predicted and actual values
#'
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#'
#' @return A data frame containing TP, TN, FP, and FN counts
#'
#' @examples
#' \dontrun{
#' obs = c(1, -1, 1, -1)
#' pred = c(1, -1, -1, 1)
#' confusionStats = PN(obs, pred)
#' }
#'
#' @export
setGeneric("PN", function(obs, pred) standardGeneric("PN"))


#' @title ROC Curve Coordinates
#'
#' @description Calculates the coordinates for a Receiver Operating Characteristic (ROC) curve
#'
#' @param labels Binary vector of true labels
#' @param scores Numeric vector of prediction scores
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item TPR - True Positive Rate (Sensitivity)
#'     \item FPR - False Positive Rate (1-Specificity)
#'     \item labels - Ordered labels
#'     \item reference - Sorted scores
#'   }
#'
#' @examples
#' \dontrun{
#' labels = c(1,0,1,1,0)
#' scores = c(0.9, 0.1, 0.8, 0.7, 0.3)
#' rocCoords = rocFn(labels, scores)
#' }
#'
#' @export
setGeneric("rocFn", function(labels, scores) standardGeneric("rocFn"))

#' Calculate ROC (Receiver Operating Characteristic) statistics for two numeric vectors.
#'
#' This function calculates ROC statistics, including True Positive Rate (TPR), False Positive Rate (FPR), True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN), and True Skill Score (TSS) for two numeric vectors.
#'
#' @param state A numeric vector representing the state values.
#' @param indicator A numeric vector representing the indicator values.
#' 
#' @return A data frame containing the following columns:
#'   \describe{
#'     \item{state}{The state values.}
#'     \item{label}{A logical vector indicating whether each state is greater than 1 (TRUE) or not (FALSE).}
#'     \item{indicator}{The indicator values.}
#'     \item{TPR}{The True Positive Rate (TPR) calculated as TP / (TP + FN).}
#'     \item{FPR}{The False Positive Rate (FPR) calculated as FP / (FP + TN).}
#'     \item{TP}{The True Positives (TP).}
#'     \item{TN}{The True Negatives (TN).}
#'     \item{FP}{The False Positives (FP).}
#'     \item{FN}{The False Negatives (FN).}
#'     \item{TSS}{The True Skill Score (TSS) calculated as (TP / (TP + FN)) - (FP / (FP + TN)).}
#'     \item{order}{The order of the indicator values after sorting in descending order.}
#'   }
#' 
#' @export
setGeneric("roc2", function(state, ind, ...) {
  standardGeneric("roc2")
})

#' @title Skill Score Calculation
#' @description Calculate prediction skill scores for fishery stock assessment models
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @param reference Reference value for classification
#' @param threshold Threshold for classification
#' @param ci Logical, calculate confidence intervals via bootstrap (default=FALSE)
#' @param ciLevel Confidence level for intervals (default=0.95)
#' @param nBoot Number of bootstrap samples if ci=TRUE (default=1000)
#' @param seed Random seed for bootstrap reproducibility (default=NULL)
#' @return Data.frame with skill metrics and optional confidence intervals
#' @export
setGeneric("skillScore", function(obs, pred, reference = NULL, threshold = 1,
                                   ci = FALSE, ciLevel = 0.95, nBoot = 1000, seed = NULL) {
  standardGeneric("skillScore")
})

#' @title Skill Summary
#' @description Comprehensive performance metrics for fishery management procedures
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @param reference Reference value for classification (default=NULL, uses optimal)
#' @param ci Logical, calculate confidence intervals via bootstrap (default=FALSE)
#' @param ciLevel Confidence level for intervals (default=0.95)
#' @param nBoot Number of bootstrap samples if ci=TRUE (default=1000)
#' @param seed Random seed for bootstrap reproducibility (default=NULL)
#' @param ... Additional arguments passed to methods
#' @return Data.frame with comprehensive skill metrics and optional confidence intervals
#' @export
setGeneric("skillSummary", function(obs, pred, reference = NULL, threshold = 1, ci = FALSE,
                                     ciLevel = 0.95, nBoot = 1000, seed = NULL, ...) {
  standardGeneric("skillSummary")
})

#' @title Trend Analysis
#' @description Calculate trend agreement metrics between time series
#' @param obs Numeric vector of observed time series
#' @param pred Numeric vector of predicted time series
#' @return Data.frame with trend metrics
#' @export
setGeneric("trend", function(obs, pred) {
  standardGeneric("trend")
})

#' @title State Classification
#' @description Evaluate stock status classification performance
#' @param obs Numeric vector of observed stock status
#' @param pred Numeric vector of predicted stock status
#' @return Data.frame with classification metrics
#' @export
setGeneric("state", function(obs, pred) {
  standardGeneric("state")
})

#' @title Variability Comparison
#' @description Compare variability characteristics between time series
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @return Data.frame with variability ratios
#' @export
setGeneric("variability", function(obs, pred) {
  standardGeneric("variability")
})

#' @title Time Series Comparison
#' @description Calculate similarity measures between time series
#' @param obs Numeric vector of observed time series
#' @param pred Numeric vector of predicted time series
#' @return Data.frame with similarity metrics
#' @export
setGeneric("compareTS", function(obs, pred) {
  standardGeneric("compareTS")
})

#' @title Cross-Correlation Function
#' @description Find optimal lag between time series
#' @param obs Numeric vector of observed time series
#' @param pred Numeric vector of predicted time series
#' @param lagMax Maximum lag to consider
#' @return Data.frame with optimal lag and ACF value
#' @export
setGeneric("ccfFn", function(obs, pred, lagMax = 5) {
  standardGeneric("ccfFn")
})

#' @title Diagnostics
#' @description Comprehensive diagnostic evaluation
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @param nDemb Embedding dimension for permutation entropy
#' @return Data.frame with diagnostic metrics
#' @export
setGeneric("diagnostics", function(obs, pred, nDemb = 5) {
  standardGeneric("diagnostics")
})

#' @title Skill Plot
#' @description Generates flexible diagnostic plots for management procedure evaluation.
#' Supports multiple input types (numeric vectors, data.frame columns, FLQuants) and 
#' customizable visualization panels (density, scatter, ROC curves).
#' @param obs Observed values (numeric vector, character column name with data.frame, or FLQuant)
#' @param pred Predicted values (numeric vector, character column name with data.frame, or FLQuant)
#' @param ... Additional arguments passed to methods (data, group, threshold, reference, panels, etc.)
#' @return ggplot object with diagnostic visualization panels
#' @export
setGeneric("skillPlot", function(obs, pred, ...) {
  standardGeneric("skillPlot")
})


