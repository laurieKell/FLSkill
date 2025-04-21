setMethod("roc2",
          signature(state = "numeric", ind = "numeric"),
          function(state, ind, ...) {
            order = order(ind, decreasing = TRUE)
            state = state[order]
            ind = ind[order]
            label = state > 1
            
            cumn = seq(1, length(label))
            
            TPR = cumsum(label) / sum(label)
            FPR = cumsum(!label) / sum(!label)
            
            TP = cumsum(label)
            FP = cumsum(!label)
            
            TN = sum(!label) - FP
            FN = sum(label) - TP
            
            TSS = (TP / (TP + FN) - FP / (FP + TN))
            
            result_df = data.frame(
              state = state,
              label = label,
              ind = ind,
              TPR = TPR,
              FPR = FPR,
              TP = TP,
              TN = TN,
              FP = FP,
              FN = FN,
              TSS = TSS,
              order = order
            )
            
            return(result_df)
          })

#' @examples
#' # In this example, we first generate sample data for state and indicator vectors. 
#' # Generate sample data
#' state <- c(0.5, 2.3, 1.2, 1.8, 3.0, 0.7)
#' indicator <- c(0.6, 2.2, 1.1, 1.9, 2.8, 0.5)
#'
#' # Then, we call the roc function to calculate ROC statistics and print the results.
#' # Calculate ROC statistics
#' roc_result=roc(state, indicator)
#'
#' # Print the ROC statistics
#' roc_result
#'
#' #Plot the ROC curve using the ggplot2 package. 
#' 
#' library(ggplot2)
#' ggplot(roc_result, aes(x = FPR, y = TPR)) +
#'   geom_line() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
#'   ggtitle("ROC Curve") +
#'   theme_minimal()
#'   
#'   

#' @rdname rocFn
#' @export
setMethod("rocFn", signature(labels="numeric", ind="numeric"),
          function(labels, ind) {
            labels <- labels[order(ind, decreasing=TRUE)]
            data.frame(TPR=cumsum(labels)/sum(labels),
                       FPR=cumsum(!labels)/sum(!labels),
                       labels,
                       reference=sort(ind))
          })

#' @rdname PN
#' @export
setMethod("PN", signature(x="numeric", y="numeric"),
          function(x, y) {
            data.frame(TP=sum(x>=0 & y>=0),
                       TN=sum(x <0 & y< 0),
                       FP=sum(x>=0 & y< 0),
                       FN=sum(x <0 & y>=0))
          })

#' @rdname TSS
#' @export
setMethod("TSS", signature(TP="numeric", TN="numeric", FP="numeric", FN="numeric"),
          function(TP, TN, FP, FN) {
            # Calculate sensitivity (true positive rate)
            sensitivity <- TP/(TP+FN)
            
            # Calculate specificity (true negative rate)
            specificity <- TN/(TN+FP)
            
            # Calculate TSS
            tss <- sensitivity + specificity - 1
            
            return(tss)
          })

