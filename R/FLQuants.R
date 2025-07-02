#' @title FLQuant Methods for FLSkill Functions
#' @description Methods for FLQuant objects from the FLR package to work with FLSkill functions
#' @importFrom FLCore FLQuant
#' @importFrom methods setMethod

#' @rdname TSS
#' @export
setMethod("TSS", signature(TP="FLQuant", TN="FLQuant", FP="FLQuant", FN="FLQuant"),
          function(TP, TN, FP, FN) {
            # Extract numeric values from FLQuant
            tp_val=as.numeric(TP)
            tn_val=as.numeric(TN)
            fp_val=as.numeric(FP)
            fn_val=as.numeric(FN)
            
            # Calculate TSS
            sensitivity=tp_val/(tp_val+fn_val)
            specificity=tn_val/(tn_val+fp_val)
            tss_val=sensitivity + specificity - 1
            
            # Return as FLQuant with same structure as input
            result=TP
            result[]=tss_val
            return(result)
          })

#' @rdname PN
#' @export
setMethod("PN", signature(obs="FLQuant", hat="FLQuant"),
          function(obs, hat) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            hat_vals=as.numeric(hat)
            
            # Calculate confusion matrix elements
            tp=sum(obs_vals >= 0 & hat_vals >= 0)
            tn=sum(obs_vals < 0 & hat_vals < 0)
            fp=sum(obs_vals >= 0 & hat_vals < 0)
            fn=sum(obs_vals < 0 & hat_vals >= 0)
            
            return(data.frame(TP=tp, TN=tn, FP=fp, FN=fn))
          })

#' @rdname rocFn
#' @export
setMethod("rocFn", signature(labels="FLQuant", ind="FLQuant"),
          function(labels, ind) {
            # Extract numeric values
            labels_vals=as.logical(labels)
            ind_vals=as.numeric(ind)
            
            # Order by indicator values (descending)
            ord=order(ind_vals, decreasing=TRUE)
            labels_ordered=labels_vals[ord]
            
            # Calculate ROC coordinates
            tpr=cumsum(labels_ordered) / sum(labels_ordered)
            fpr=cumsum(!labels_ordered) / sum(!labels_ordered)
            
            return(data.frame(
              TPR = tpr,
              FPR = fpr,
              labels = labels_ordered,
              reference = sort(ind_vals, decreasing=TRUE)
            ))
          })

#' @rdname roc2
#' @export
setMethod("roc2", signature(state="FLQuant", ind="FLQuant"),
          function(state, ind, ...) {
            # Extract numeric values
            state_vals=as.numeric(state)
            ind_vals=as.numeric(ind)
            
            # Order by indicator values (descending)
            ord=order(ind_vals, decreasing=TRUE)
            state_ordered=state_vals[ord]
            ind_ordered=ind_vals[ord]
            label=state_ordered > 1
            
            # Calculate ROC statistics
            tpr=cumsum(label) / sum(label)
            fpr=cumsum(!label) / sum(!label)
            
            tp=cumsum(label)
            fp=cumsum(!label)
            tn=sum(!label) - fp
            fn=sum(label) - tp
            
            tss=(tp / (tp + fn) - fp / (fp + tn))
            
            return(data.frame(
              state = state_ordered,
              label = label,
              ind = ind_ordered,
              TPR = tpr,
              FPR = fpr,
              TP = tp,
              TN = tn,
              FP = fp,
              FN = fn,
              TSS = tss,
              order = ord
            ))
          })

#' @title Skill Score for FLQuant
#' @description Calculate skill scores for FLQuant objects
#' @param obs FLQuant object of observed values
#' @param pred FLQuant object of predicted values
#' @param reference Reference value for classification (default=1)
#' @param threshold Threshold for classification (default=1)
#' @return Data.frame with skill metrics
#' @export
setMethod("skillScore", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred, reference = NULL, threshold = 1) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            skillScore(obs_vals, pred_vals, reference, threshold)
          })

#' @title Skill Summary for FLQuant
#' @description Calculate comprehensive skill summary for FLQuant objects
#' @param obs FLQuant object of observed values
#' @param pred FLQuant object of predicted values
#' @return Data.frame with comprehensive skill metrics
#' @export
setMethod("skillSummary", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            skillSummary(obs_vals, pred_vals)
          })

#' @title Trend Analysis for FLQuant
#' @description Calculate trend agreement metrics for FLQuant objects
#' @param obs FLQuant object of observed time series
#' @param pred FLQuant object of predicted time series
#' @return Data.frame with trend metrics
#' @export
setMethod("trend", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            trend(obs_vals, pred_vals)
          })

#' @title State Classification for FLQuant
#' @description Evaluate stock status classification for FLQuant objects
#' @param obs FLQuant object of observed stock status
#' @param pred FLQuant object of predicted stock status
#' @return Data.frame with classification metrics
#' @export
setMethod("state", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            state(obs_vals, pred_vals)
          })

#' @title Variability Comparison for FLQuant
#' @description Compare variability characteristics for FLQuant objects
#' @param obs FLQuant object of observed values
#' @param pred FLQuant object of predicted values
#' @return Data.frame with variability ratios
#' @export
setMethod("variability", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            variability(obs_vals, pred_vals)
          })

#' @title Time Series Comparison for FLQuant
#' @description Calculate similarity measures for FLQuant time series
#' @param obs FLQuant object of observed time series
#' @param pred FLQuant object of predicted time series
#' @return Data.frame with similarity metrics
#' @export
setMethod("compareTS", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            compareTS(obs_vals, pred_vals)
          })

#' @title Cross-Correlation Function for FLQuant
#' @description Find optimal lag between FLQuant time series
#' @param obs FLQuant object of observed time series
#' @param pred FLQuant object of predicted time series
#' @param lag.max Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and ACF value
#' @export
setMethod("ccfFn", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred, lag.max = 5) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            ccfFn(obs_vals, pred_vals, lag.max)
          })

#' @title Diagnostics for FLQuant
#' @description Comprehensive diagnostic evaluation for FLQuant objects
#' @param obs FLQuant object of observed values
#' @param pred FLQuant object of predicted values
#' @param ndemb Embedding dimension for permutation entropy (default=5)
#' @return Data.frame with diagnostic metrics
#' @export
setMethod("diagnostics", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred, ndemb = 5) {
            # Extract numeric values
            obs_vals=as.numeric(obs)
            pred_vals=as.numeric(pred)
            
            # Call the numeric method
            diagnostics(obs_vals, pred_vals, ndemb)
          }) 