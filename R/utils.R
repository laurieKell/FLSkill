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


