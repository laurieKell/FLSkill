#dat=data.frame(om=rep(c(1,2),10),mp=rlnorm(20))

#aucTss(dat$om>1,dat$mp)

#' @title Trend Agreement Metrics
#' @description Calculates correlation measures between observed and predicted stock trends.
#' @param obs Numeric vector of observed time series (e.g., biomass index)
#' @param pred Numeric vector of predicted time series
#' @return Data.frame containing:
#' \itemize{
#'   \item pearson: Pearson correlation coefficient
#'   \item spearman: Spearman's rank correlation  
#'   \item direction: Proportion of matching inter-annual change directions
#' }
#' @examples
#' obs <- cumsum(rnorm(20))
#' pred <- obs + rnorm(20, sd=0.5)
#' trend(obs, pred)
#' @export
trend <- function(obs, pred) {
  data.frame(
    pearson = cor(pred, obs, method = "pearson"),
    spearman = cor(pred, obs, method = "spearman"),
    direction = mean(sign(diff(pred)) == sign(diff(obs)), na.rm = TRUE)
  )
}


#' @title Stock Status Classification Metrics
#' @description Evaluates performance of stock status classification (Overfished/Healthy).
#' @param obs Numeric vector of observed stock status values
#' @param pred Numeric vector of predicted status values
#' @return Data.frame containing:
#' \itemize{
#'   \item accuracy: Overall classification accuracy
#'   \item precision: Precision for Overfished classification
#'   \item recall: Recall for Overfished classification
#' }
#' @note Status determined using threshold at 1 (B/BMSY). Check variable names in function code.
#' @examples
#' obs <- runif(100, 0.5, 1.5)
#' pred <- obs * exp(rnorm(100, sd=0.2))
#' state(obs, pred)
#' @export
state <- function(obs, pred) {
  obs_status <- ifelse(obs < 1, "Overfished", "Healthy")
  pred_status <- ifelse(pred < 1, "Overfished", "Healthy")
  data.frame(
    accuracy = mean(obs_status == pred_status),
    precision = sum(obs_status == "Overfished" & pred_status == "Overfished") / sum(pred_status == "Overfished"),
    recall = sum(obs_status == "Overfished" & pred_status == "Overfished") / sum(obs_status == "Overfished")
  )
}


#' @title Variability Comparison Metrics
#' @description Compares variability characteristics between observed and predicted time series.
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values  
#' @return Data.frame containing variability ratios:
#' \itemize{
#'   \item sd: Standard deviation ratio (pred/obs)
#'   \item iqr: Interquartile range ratio
#'   \item cv: Coefficient of variation ratio
#' }
#' @examples 
#' obs <- rlnorm(100, meanlog=log(1), sdlog=0.4)
#' pred <- obs * exp(rnorm(100, sd=0.1))
#' variability(obs, pred)
#' @export
variability <- function(obs, pred) {
  data.frame(
    sd = sd(pred) / sd(obs),
    iqr = IQR(pred) / IQR(obs),
    cv = (sd(pred) / mean(pred)) / (sd(obs) / mean(obs))
  )
}


#' @title Comprehensive Diagnostic Evaluation
#' @description Integrates multiple performance metrics for stock assessment model validation.
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @param ndemb Embedding dimension for permutation entropy calculation (default=5)
#' @return Data.frame containing 10 diagnostic metrics. Final classification currently non-functional.
#' @note Requires helper functions: stdz(), permutation_entropy(), ordinal_pattern_distribution()
#' @examples
#' obs <- runif(100, 0.8, 1.2)
#' pred <- obs * exp(rnorm(100, sd=0.15))
#' diagnostics(obs, pred)
#' @export
diagnostics <- function(obs, pred, ndemb = 5) {
  roc = rocFn(stdz(obs) > 1, stdz(pred))
  tss = skillScore(stdz(pred), stdz(obs) - 1)
  
  return(
    data.frame(
      trend     = cor(pred, obs),
      status    = mean((pred < 1) == (obs < 1)),
      sd.pred   = sd(pred),
      sd.obs    = sd(obs),
      variability = sd(pred) / sd(obs),
      auc       = FLCore:::auc(TPR = roc$TPR, FPR = roc$FPR),
      tss       = tss$TSS,
      fpr       = tss$FPR,
      tpr       = tss$TPR,
      entropy   = permutation_entropy(ordinal_pattern_distribution(obs, ndemb = ndemb))
    )
  )
  
  case_when(
    trend > 0.7 & status > 0.8 & between(variability,0.8,1.2) ~ "Excellent",
    trend > 0.5 & status > 0.7 & between(variability,0.6,1.4) ~ "Adequate",
    TRUE ~ "Needs Improvement")
}


#' @title Time Series Comparison Metrics
#' @description Calculates similarity measures between two stock assessment time series.
#' @param obs Numeric vector (e.g., observed biomass index)
#' @param pred Numeric vector (e.g., model-predicted biomass)
#' @return Data.frame with:
#' \itemize{
#'   \item rmse: Root Mean Square Error
#'   \item correlation: Pearson correlation
#'   \item sd: Standard deviation of residuals
#' }
#' @examples
#' obs <- cumsum(rnorm(20))
#' pred <- obs + rnorm(20, sd=0.5)
#' compareTS(obs, pred)
#' @export
compareTS <- function(obs, pred) {
  min_length <- min(length(obs), length(pred))
  obs <- obs[1:min_length]
  pred <- pred[1:min_length]
  rmse <- sqrt(mean((obs - pred)^2))
  correlation <- cor(obs, pred)
  results <- data.frame(
    rmse = rmse,
    correlation = correlation,
    sd = sd(pred - obs)
  )
  return(results)
}

#' @title Optimal Lag Finder
#' @description Identifies lag with maximum cross-correlation between stock assessment time series.
#' @param obs Observed time series (e.g., survey index)
#' @param pred Predicted time series (e.g., model output)
#' @param lag.max Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and corresponding ACF value
#' @examples
#' obs <- sin(seq(0, 2*pi, length=50)) + rnorm(50)
#' pred <- lag(obs, 2) + rnorm(50, sd=0.2)
#' ccfFn(obs, pred)
#' @export
ccfFn <- function(obs, pred, lag.max = 5) {
  rtn <- ccf(obs, pred, plot = FALSE, lag.max = lag.max)
  subset(data.frame(lag = rtn$lag, acf = rtn$acf), acf == max(acf))
}

#' @title True Skill Statistic (TSS) Calculator
#' @description Calculates the True Skill Statistic for binary classification in stock assessments.
#' @param TP True Positives (correct overfished predictions)
#' @param TN True Negatives (correct healthy stock predictions)
#' @param FP False Positives (false overfished predictions)
#' @param FN False Negatives (false healthy stock predictions)
#' @return Numeric TSS value ranging from -1 to +1 (perfect skill)
#' @examples
#' TSS(TP = 45, TN = 30, FP = 10, FN = 15) # Good skill (0.5)
#' @export
TSS <- function(TP, TN, FP, FN) TP / (FN + TP) - TN / (FP + TN)


#' @title Area Under the Curve (AUC) via Trapezoidal Rule
#' @description Computes the area under a curve (AUC) using the trapezoidal rule. Used for ROC curves.
#' @param TPR Numeric vector of true positive rates (y-axis)
#' @param FPR Numeric vector of false positive rates (x-axis)
#' @return Numeric value of AUC
#' @keywords internal
auc_trapz <- function(TPR, FPR) {
  ord <- order(FPR)
  TPR <- TPR[ord]
  FPR <- FPR[ord]
  sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)
}

#' @title Basic Confusion Matrix
#' @description Computes basic confusion matrix statistics for binary classification.
#' @param obs Logical or binary vector of observed values
#' @param pred Logical or binary vector of predicted values
#' @return Data.frame with TP, TN, FP, FN
#' @keywords internal
conf_matrix_basic <- function(obs, pred) {
  data.frame(
    TP = sum(obs & pred),
    TN = sum(!obs & !pred),
    FP = sum(!obs & pred),
    FN = sum(obs & !pred)
  )
}


