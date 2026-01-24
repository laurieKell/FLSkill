#dat=data.frame(om=rep(c(1,2),10),mp=rlnorm(20))

#aucTss(dat$om>1,dat$mp)

#' @rdname trend
#' @export
setMethod("trend", signature(response="numeric", predictor="numeric"),
          function(response, predictor) {
            data.frame(
              pearson = cor(predictor, response, method = "pearson"),
              spearman = cor(predictor, response, method = "spearman"),
              direction = mean(sign(diff(predictor)) == sign(diff(response)), na.rm = TRUE)
            )
          })


#' @rdname state
#' @export
setMethod("state", signature(response="numeric", predictor="numeric"),
          function(response, predictor) {
            response_status=ifelse(response < 1, "Overfished", "Healthy")
            predictor_status=ifelse(predictor < 1, "Overfished", "Healthy")
            data.frame(
              accuracy = mean(response_status == predictor_status),
              precision = sum(response_status == "Overfished" & predictor_status == "Overfished") / sum(predictor_status == "Overfished"),
              recall = sum(response_status == "Overfished" & predictor_status == "Overfished") / sum(response_status == "Overfished")
            )
          })


#' @rdname variability
#' @export
setMethod("variability", signature(response="numeric", predictor="numeric"),
          function(response, predictor) {
            data.frame(
              sd = sd(predictor) / sd(response),
              iqr = IQR(predictor) / IQR(response),
              cv = (sd(predictor) / mean(predictor)) / (sd(response) / mean(response))
            )
          })


#' @rdname diagnostics
#' @export
setMethod("diagnostics", signature(response="numeric", predictor="numeric"),
          function(response, predictor, nDemb = 5) {
            # Standardize data (z-score normalization)
            stdz = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
            roc = rocFn(stdz(response) > 1, stdz(predictor))
            tss = skillScore(stdz(predictor), stdz(response) - 1)
            
            return(
              data.frame(
                trend     = cor(predictor, response),
                status    = mean((predictor < 1) == (response < 1)),
                sd.pred   = sd(predictor),
                sd.obs    = sd(response),
                variability = sd(predictor) / sd(response),
                auc       = FLCore:::auc(TPR = roc$TPR, FPR = roc$FPR),
                tss       = tss$TSS,
                fpr       = tss$FPR,
                tpr       = tss$TPR,
                entropy   = permutation_entropy(ordinal_pattern_distribution(response, ndemb = nDemb))
              )
            )
            
            case_when(
              trend > 0.7 & status > 0.8 & between(variability,0.8,1.2) ~ "Excellent",
              trend > 0.5 & status > 0.7 & between(variability,0.6,1.4) ~ "Adequate",
              TRUE ~ "Needs Improvement")
          })


#' @rdname compareTS
#' @export
setMethod("compareTS", signature(response="numeric", predictor="numeric"),
          function(response, predictor) {
            min_length=min(length(response), length(predictor))
            response=response[1:min_length]
            predictor=predictor[1:min_length]
            rmse=sqrt(mean((response - predictor)^2))
            correlation=cor(response, predictor)
            results=data.frame(
              rmse = rmse,
              correlation = correlation,
              sd = sd(predictor - response)
            )
            return(results)
          })

#' @rdname ccfFn
#' @export
setMethod("ccfFn", signature(response="numeric", predictor="numeric"),
          function(response, predictor, lagMax = 5) {
            rtn=ccf(response, predictor, plot = FALSE, lag.max = lagMax)
            subset(data.frame(lag = rtn$lag, acf = rtn$acf), acf == max(acf))
          })

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
setMethod("TSS", signature(TP="numeric",TN="numeric",FP="numeric",FN="numeric"),
          function(TP,TN,FP,FN) TP/(FN+TP)-TN/(FP+TN)
          )

#' @title Area Under the Curve (AUC) via Trapezoidal Rule
#' @description Computes the area under a curve (AUC) using the trapezoidal rule. Used for ROC curves.
#' @param TPR Numeric vector of true positive rates (y-axis)
#' @param FPR Numeric vector of false positive rates (x-axis)
#' @return Numeric value of AUC
#' @keywords internal
auc_trapz<-function(TPR, FPR) {
  ord=order(FPR)
  TPR=TPR[ord]
  FPR=FPR[ord]
  sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)
}

#' @title Basic Confusion Matrix
#' @description Computes basic confusion matrix statistics for binary classification.
#' @param response Logical or binary vector of observed values
#' @param predictor Logical or binary vector of predicted values
#' @return Data.frame with TP, TN, FP, FN
#' @keywords internal
conf_matrix_basic<-function(response, predictor) {
  data.frame(
    TP = sum(response & predictor),
    TN = sum(!response & !predictor),
    FP = sum(!response & predictor),
    FN = sum(response & !predictor)
  )
}

#' @title Bootstrap Confidence Intervals for Skill Metrics
#' @description Calculates bootstrap confidence intervals for AUC, TSS, TPR, and FPR.
#' @param response Numeric vector of observed values
#' @param predictor Numeric vector of predicted values
#' @param threshold Threshold for classification (default=1)
#' @param reference Reference value for classification (default=1)
#' @param nBoot Number of bootstrap samples (default=1000)
#' @param ciLevel Confidence level (default=0.95)
#' @param seed Random seed for reproducibility (default=NULL)
#' @return Data.frame with metric names, estimates, and CI bounds
#' @keywords internal
bootstrapSkillMetricsCI <- function(response, predictor, threshold = 1, reference = 1,
                                     nBoot = 1000, ciLevel = 0.95, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Remove NAs
  valid = !is.na(response) & !is.na(predictor)
  response = response[valid]
  predictor = predictor[valid]

  n = length(response)
  if (n < 10) {
    warning("Sample size too small for bootstrap CI calculation")
    return(data.frame(
      metric = c("AUC", "TSS", "TPR", "FPR"),
      estimate = NA_real_,
      ciLower = NA_real_,
      ciUpper = NA_real_
    ))
  }

  # Calculate original metrics (rocFn2 is in same package, call directly)
  rocs = rocFn2(response > threshold, predictor)
  aucOrig = auc_trapz(rocs$TPR, rocs$FPR)
  
  # Find reference point
  flag = which.min(abs(rocs$predictor - reference))
  tssOrig = rocs$TPR[flag] - rocs$FPR[flag]
  tprOrig = rocs$TPR[flag]
  fprOrig = rocs$FPR[flag]

  # Bootstrap samples
  bootAuc = numeric(nBoot)
  bootTss = numeric(nBoot)
  bootTpr = numeric(nBoot)
  bootFpr = numeric(nBoot)
  
  for (i in 1:nBoot) {
    # Resample with replacement
    idx = sample(n, n, replace = TRUE)
    responseBoot = response[idx]
    predictorBoot = predictor[idx]
    
    # Calculate metrics for bootstrap sample
    tryCatch({
      rocsBoot = rocFn2(responseBoot > threshold, predictorBoot)
      
      # AUC
      if (length(rocsBoot$TPR) > 1 && length(rocsBoot$FPR) > 1) {
        bootAuc[i] = auc_trapz(rocsBoot$TPR, rocsBoot$FPR)
      } else {
        bootAuc[i] = NA_real_
      }
      
      # Find reference point
      flagBoot = which.min(abs(rocsBoot$predictor - reference))
      if (length(flagBoot) > 0 && flagBoot[1] <= length(rocsBoot$TPR)) {
        bootTss[i] = rocsBoot$TPR[flagBoot[1]] - rocsBoot$FPR[flagBoot[1]]
        bootTpr[i] = rocsBoot$TPR[flagBoot[1]]
        bootFpr[i] = rocsBoot$FPR[flagBoot[1]]
      } else {
        bootTss[i] = NA_real_
        bootTpr[i] = NA_real_
        bootFpr[i] = NA_real_
      }
    }, error = function(e) {
      bootAuc[i] = NA_real_
      bootTss[i] = NA_real_
      bootTpr[i] = NA_real_
      bootFpr[i] = NA_real_
    })
  }
  
  # Calculate percentiles for CI
  alpha = 1 - ciLevel
  lowerPercentile = alpha / 2
  upperPercentile = 1 - alpha / 2
  
  # Function to calculate CI, handling NAs
  calcCI <- function(bootValues, original) {
    bootClean = bootValues[!is.na(bootValues)]
    if (length(bootClean) < 10) {
      return(c(NA_real_, NA_real_))
    }
    quantile(bootClean, probs = c(lowerPercentile, upperPercentile), na.rm = TRUE)
  }
  
  # Calculate CIs
  aucCI = calcCI(bootAuc, aucOrig)
  tssCI = calcCI(bootTss, tssOrig)
  tprCI = calcCI(bootTpr, tprOrig)
  fprCI = calcCI(bootFpr, fprOrig)
  
  # Return results
  data.frame(
    metric = c("AUC", "TSS", "TPR", "FPR"),
    estimate = c(aucOrig, tssOrig, tprOrig, fprOrig),
    ciLower = c(aucCI[1], tssCI[1], tprCI[1], fprCI[1]),
    ciUpper = c(aucCI[2], tssCI[2], tprCI[2], fprCI[2]),
    stringsAsFactors = FALSE
  )
}


