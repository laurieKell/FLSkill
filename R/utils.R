#dat=data.frame(om=rep(c(1,2),10),mp=rlnorm(20))

#aucTss(dat$om>1,dat$mp)

#' @rdname trend
#' @export
setMethod("trend", signature(obs="numeric", pred="numeric"),
          function(obs, pred) {
            data.frame(
              pearson = cor(pred, obs, method = "pearson"),
              spearman = cor(pred, obs, method = "spearman"),
              direction = mean(sign(diff(pred)) == sign(diff(obs)), na.rm = TRUE)
            )
          })


#' @rdname state
#' @export
setMethod("state", signature(obs="numeric", pred="numeric"),
          function(obs, pred) {
            obs_status=ifelse(obs < 1, "Overfished", "Healthy")
            pred_status=ifelse(pred < 1, "Overfished", "Healthy")
            data.frame(
              accuracy = mean(obs_status == pred_status),
              precision = sum(obs_status == "Overfished" & pred_status == "Overfished") / sum(pred_status == "Overfished"),
              recall = sum(obs_status == "Overfished" & pred_status == "Overfished") / sum(obs_status == "Overfished")
            )
          })


#' @rdname variability
#' @export
setMethod("variability", signature(obs="numeric", pred="numeric"),
          function(obs, pred) {
            data.frame(
              sd = sd(pred) / sd(obs),
              iqr = IQR(pred) / IQR(obs),
              cv = (sd(pred) / mean(pred)) / (sd(obs) / mean(obs))
            )
          })


#' @rdname diagnostics
#' @export
setMethod("diagnostics", signature(obs="numeric", pred="numeric"),
          function(obs, pred, ndemb = 5) {
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
          })


#' @rdname compareTS
#' @export
setMethod("compareTS", signature(obs="numeric", pred="numeric"),
          function(obs, pred) {
            min_length=min(length(obs), length(pred))
            obs=obs[1:min_length]
            pred=pred[1:min_length]
            rmse=sqrt(mean((obs - pred)^2))
            correlation=cor(obs, pred)
            results=data.frame(
              rmse = rmse,
              correlation = correlation,
              sd = sd(pred - obs)
            )
            return(results)
          })

#' @rdname ccfFn
#' @export
setMethod("ccfFn", signature(obs="numeric", pred="numeric"),
          function(obs, pred, lag.max = 5) {
            rtn=ccf(obs, pred, plot = FALSE, lag.max = lag.max)
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
#' @param obs Logical or binary vector of observed values
#' @param pred Logical or binary vector of predicted values
#' @return Data.frame with TP, TN, FP, FN
#' @keywords internal
conf_matrix_basic<-function(obs, pred) {
  data.frame(
    TP = sum(obs & pred),
    TN = sum(!obs & !pred),
    FP = sum(!obs & pred),
    FN = sum(obs & !pred)
  )
}

#' @title Bootstrap Confidence Intervals for Skill Metrics
#' @description Calculates bootstrap confidence intervals for AUC, TSS, TPR, and FPR.
#' @param obs Numeric vector of observed values
#' @param pred Numeric vector of predicted values
#' @param threshold Threshold for classification (default=1)
#' @param reference Reference value for classification (default=1)
#' @param nBoot Number of bootstrap samples (default=1000)
#' @param ciLevel Confidence level (default=0.95)
#' @param seed Random seed for reproducibility (default=NULL)
#' @return Data.frame with metric names, estimates, and CI bounds
#' @keywords internal
bootstrapSkillMetricsCI <- function(obs, pred, threshold = 1, reference = 1,
                                     nBoot = 1000, ciLevel = 0.95, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Remove NAs
  valid = !is.na(obs) & !is.na(pred)
  obs = obs[valid]
  pred = pred[valid]
  
  n = length(obs)
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
  rocs = rocFn2(obs > threshold, pred)
  aucOrig = auc_trapz(rocs$TPR, rocs$FPR)
  
  # Find reference point
  flag = which.min(abs(rocs$pred - reference))
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
    obsBoot = obs[idx]
    predBoot = pred[idx]
    
    # Calculate metrics for bootstrap sample
    tryCatch({
      rocsBoot = rocFn2(obsBoot > threshold, predBoot)
      
      # AUC
      if (length(rocsBoot$TPR) > 1 && length(rocsBoot$FPR) > 1) {
        bootAuc[i] = auc_trapz(rocsBoot$TPR, rocsBoot$FPR)
      } else {
        bootAuc[i] = NA_real_
      }
      
      # Find reference point
      flagBoot = which.min(abs(rocsBoot$pred - reference))
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


