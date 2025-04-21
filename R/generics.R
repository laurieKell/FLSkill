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
#' tss_value <- TSS(TP=10, TN=8, FP=2, FN=3)
#' }
#'
#' @export
setGeneric("TSS", function(TP, TN, FP, FN) standardGeneric("TSS"))


#' @title Confusion Matrix Statistics
#'
#' @description Calculates True Positive (TP), True Negative (TN), False Positive (FP), 
#' and False Negative (FN) counts from predicted and actual values
#'
#' @param x Numeric vector of predicted values
#' @param y Numeric vector of actual values
#'
#' @return A data frame containing TP, TN, FP, and FN counts
#'
#' @examples
#' \dontrun{
#' pred <- c(1, -1, 1, -1)
#' actual <- c(1, -1, -1, 1)
#' confusion_stats <- PN(pred, actual)
#' }
#'
#' @export
setGeneric("PN", function(x, y) standardGeneric("PN"))


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
#' labels <- c(1,0,1,1,0)
#' scores <- c(0.9, 0.1, 0.8, 0.7, 0.3)
#' roc_coords <- rocFn(labels, scores)
#' }
#'
#' @export
setGeneric("rocFn", function(labels, ind) standardGeneric("rocFn"))

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


