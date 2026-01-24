setMethod("roc2",
          signature(state = "numeric", ind = "numeric"),
          function(state, ind, ...) {
            ord = order(ind, decreasing = TRUE)
            state = state[ord]
            ind = ind[ord]
            label = state > 1
            
            # Calculate ROC statistics
            tpr = cumsum(label) / sum(label)
            fpr = cumsum(!label) / sum(!label)
            
            tp = cumsum(label)
            fp = cumsum(!label)
            tn = sum(!label) - fp
            fn = sum(label) - tp
            
            tss = (tp / (tp + fn) - fp / (fp + tn))
            
            resultDf = data.frame(
              state = state,
              label = label,
              ind = ind,
              TPR = tpr,
              FPR = fpr,
              TP = tp,
              TN = tn,
              FP = fp,
              FN = fn,
              TSS = tss,
              order = ord
            )
            
            return(resultDf)
          })

#' @examples
#' # In this example, we first generate sample data for state and indicator vectors. 
#' # Generate sample data
#' state = c(0.5, 2.3, 1.2, 1.8, 3.0, 0.7)
#' indicator = c(0.6, 2.2, 1.1, 1.9, 2.8, 0.5)
#'
#' # Then, we call the roc function to calculate ROC statistics and print the results.
#' # Calculate ROC statistics
#' rocResult = roc2(state, indicator)
#'
#' # Print the ROC statistics
#' rocResult
#'
#' #Plot the ROC curve using the ggplot2 package. 
#' 
#' library(ggplot2)
#' ggplot(rocResult, aes(x = FPR, y = TPR)) +
#'   geom_line() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
#'   ggtitle("ROC Curve") +
#'   theme_minimal()
#'   
#'   

#' @rdname rocFn
#' @export
setMethod("rocFn", signature(labels="logical", scores="numeric"),
          function(labels, scores) {
            labels = labels[order(scores, decreasing = TRUE)]
            data.frame(TPR = cumsum(labels) / sum(labels),
                       FPR = cumsum(!labels) / sum(!labels),
                       labels,
                       reference = sort(scores))
          })

#' @rdname PN
#' @export
setMethod("PN", signature(response="numeric", predictor="numeric"),
          function(response, predictor) {
            data.frame(TP = sum(response >= 0 & predictor >= 0),
                       TN = sum(response < 0 & predictor < 0),
                       FP = sum(response >= 0 & predictor < 0),
                       FN = sum(response < 0 & predictor >= 0))
          })

#' @rdname TSS
#' @export
setMethod("TSS", signature(TP="numeric", TN="numeric", FP="numeric", FN="numeric"),
          function(TP, TN, FP, FN) {
            # Calculate sensitivity (true positive rate)
            sensitivity = TP / (TP + FN)
            
            # Calculate specificity (true negative rate)
            specificity = TN / (TN + FP)
            
            # Calculate TSS
            tss = sensitivity + specificity - 1
            
            return(tss)
          })

specificity<-function(TPR) TPR
sensitivity<-function(FPR) FPR+1

FPR<-function(specificity) 1-specificity
TPR<-function(sensitivity) sensitivity