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
setMethod("PN", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred) {
            # Extract numeric values
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Calculate confusion matrix elements
            tp = sum(obsVals >= 0 & predVals >= 0)
            tn = sum(obsVals < 0 & predVals < 0)
            fp = sum(obsVals >= 0 & predVals < 0)
            fn = sum(obsVals < 0 & predVals >= 0)
            
            return(data.frame(TP = tp, TN = tn, FP = fp, FN = fn))
          })

#' @rdname rocFn
#' @export
setMethod("rocFn", signature(labels="FLQuant", scores="FLQuant"),
          function(labels, scores) {
            # Extract numeric values
            labelsVals = as.logical(labels)
            scoresVals = as.numeric(scores)
            
            # Order by scores values (descending)
            ord = order(scoresVals, decreasing = TRUE)
            labelsOrdered = labelsVals[ord]
            
            # Calculate ROC coordinates
            tpr = cumsum(labelsOrdered) / sum(labelsOrdered)
            fpr = cumsum(!labelsOrdered) / sum(!labelsOrdered)
            
            return(data.frame(
              TPR = tpr,
              FPR = fpr,
              labels = labelsOrdered,
              reference = sort(scoresVals, decreasing = TRUE)
            ))
          })

#' @rdname roc2
#' @export
setMethod("roc2", signature(state="FLQuant", ind="FLQuant"),
          function(state, ind, ...) {
            # Extract numeric values
            stateVals = as.numeric(state)
            indVals = as.numeric(ind)
            
            # Order by indicator values (descending)
            ord = order(indVals, decreasing = TRUE)
            stateOrdered = stateVals[ord]
            indOrdered = indVals[ord]
            label = stateOrdered > 1
            
            # Calculate ROC statistics
            tpr = cumsum(label) / sum(label)
            fpr = cumsum(!label) / sum(!label)
            
            tp = cumsum(label)
            fp = cumsum(!label)
            tn = sum(!label) - fp
            fn = sum(label) - tp
            
            tss = (tp / (tp + fn) - fp / (fp + tn))
            
            return(data.frame(
              state = stateOrdered,
              label = label,
              ind = indOrdered,
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            skillScore(obsVals, predVals, reference, threshold)
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            skillSummary(obsVals, predVals)
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            trend(obsVals, predVals)
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            state(obsVals, predVals)
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            variability(obsVals, predVals)
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
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            compareTS(obsVals, predVals)
          })

#' @title Cross-Correlation Function for FLQuant
#' @description Find optimal lag between FLQuant time series
#' @param obs FLQuant object of observed time series
#' @param pred FLQuant object of predicted time series
#' @param lagMax Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and ACF value
#' @export
setMethod("ccfFn", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred, lagMax = 5) {
            # Extract numeric values
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            ccfFn(obsVals, predVals, lagMax)
          })

#' @title Diagnostics for FLQuant
#' @description Comprehensive diagnostic evaluation for FLQuant objects
#' @param obs FLQuant object of observed values
#' @param pred FLQuant object of predicted values
#' @param nDemb Embedding dimension for permutation entropy (default=5)
#' @return Data.frame with diagnostic metrics
#' @export
setMethod("diagnostics", signature(obs="FLQuant", pred="FLQuant"),
          function(obs, pred, nDemb = 5) {
            # Extract numeric values
            obsVals = as.numeric(obs)
            predVals = as.numeric(pred)
            
            # Call the numeric method
            diagnostics(obsVals, predVals, nDemb)
          }) 