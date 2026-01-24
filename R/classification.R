#' @title Receiver Operating Characteristic (ROC) Curve Generator
#' @description Creates ROC curve data for stock status classification performance.
#' @param response Logical vector of true stock statuses (TRUE=overfished)
#' @param predictor Numeric vector of model predictions (e.g., B/BMSY ratios)
#' @return Data.frame with columns:
#' \itemize{
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item response: Ordered true labels
#'   \item predictor: Threshold values
#' }
#' @examples
#' response=runif(100, 0.5, 1.5) < 1
#' predictor=rnorm(100)
#' roc_data=rocFn2(response, predictor)
#' @export
rocFn2<-function(response, predictor) {
  ord=order(predictor, decreasing = TRUE)
  response_ordered=response[ord]
  data.frame(
    TPR = cumsum(response_ordered) / sum(response_ordered),
    FPR = cumsum(!response_ordered) / sum(!response_ordered),
    response = response_ordered,
    predictor = predictor[ord]
  )
}

#' @rdname skillScore
#' @param ci Logical, calculate confidence intervals via bootstrap (default=FALSE)
#' @param ciLevel Confidence level for intervals (default=0.95)
#' @param nBoot Number of bootstrap samples if ci=TRUE (default=1000)
#' @param seed Random seed for bootstrap reproducibility (default=NULL)
#' @details
#' When ci=TRUE, bootstrap confidence intervals are calculated for AUC, TSS, TPR, and FPR.
#' The bootstrap method resamples the data with replacement to estimate the sampling
#' distribution of each metric. Confidence intervals are added as additional columns:
#' AUC_CI_lower, AUC_CI_upper, TSS_CI_lower, TSS_CI_upper, TPR_CI_lower, TPR_CI_upper,
#' FPR_CI_lower, FPR_CI_upper.
#' @examples
#' # Basic usage
#' response = rlnorm(100, meanlog=log(1), sdlog=0.5)
#' predictor = response * exp(rnorm(100, sd=0.3))
#' skillScore(response, predictor, reference=1)
#' 
#' # With confidence intervals
#' skillScore(response, predictor, reference=1, ci=TRUE, nBoot=1000, seed=123)
#' @export
setMethod("skillScore", signature(response="logical", predictor="numeric"),
          function(response, predictor, reference = NULL, threshold = NULL, 
                   ci = FALSE, ciLevel = 0.95, nBoot = 1000, seed = NULL) {
            rocs=rocFn2(response, predictor)

            if (is.null(reference)) {
              flag=which.max(rocs$TPR - rocs$FPR)
              reference=rocs$predictor[flag]
            } else 
              flag=which.min(abs(rocs$predictor - reference))
            
            TP=sum(response & predictor >  rocs$predictor[flag])
            TN=sum(response & predictor <= rocs$predictor[flag])
            FP=sum(response & predictor <= rocs$predictor[flag])
            FN=sum(response & predictor >  rocs$predictor[flag])
        
            result = data.frame(
              AUC = auc_trapz(rocs$TPR, rocs$FPR),
              TSS = rocs$TPR[flag] - rocs$FPR[flag],
              ref = rocs$predictor[flag],
              TPR = rocs$TPR[flag],
              FPR = rocs$FPR[flag],
              TP = TP, TN = TN, FP = FP, FN = FN
            )
        
            # Add confidence intervals if requested
            if (ci) {
              ciResults = bootstrapSkillMetricsCI(response, predictor, threshold = threshold,
                                                  reference = reference, nBoot = nBoot,
                                                  ciLevel = ciLevel, seed = seed)
              
              # Add CI columns to result
              result$AUC_CI_lower = ciResults$ciLower[ciResults$metric == "AUC"]
              result$AUC_CI_upper = ciResults$ciUpper[ciResults$metric == "AUC"]
              result$TSS_CI_lower = ciResults$ciLower[ciResults$metric == "TSS"]
              result$TSS_CI_upper = ciResults$ciUpper[ciResults$metric == "TSS"]
              result$TPR_CI_lower = ciResults$ciLower[ciResults$metric == "TPR"]
              result$TPR_CI_upper = ciResults$ciUpper[ciResults$metric == "TPR"]
              result$FPR_CI_lower = ciResults$ciLower[ciResults$metric == "FPR"]
              result$FPR_CI_upper = ciResults$ciUpper[ciResults$metric == "FPR"]
            }
            
      return(result)})

setMethod("skillScore", signature(response="numeric", predictor="numeric"),
          function(response, predictor, reference = NULL, threshold = as.numeric(1),  
                   ci = FALSE, ciLevel = 0.95, nBoot = 1000, seed = NULL) {
            
          skillScore(response>threshold, predictor=predictor,reference=reference,threshold=threshold,
                     ci=ci,ciLevel=ciLevel,nBoot=nBoot,seed=seed)})
                   
                   
#' @rdname skillSummary
#' @param ci Logical, calculate confidence intervals via bootstrap (default=FALSE)
#' @param ciLevel Confidence level for intervals (default=0.95)
#' @param nBoot Number of bootstrap samples if ci=TRUE (default=1000)
#' @param seed Random seed for bootstrap reproducibility (default=NULL)
#' @details
#' When ci=TRUE, bootstrap confidence intervals are calculated for AUC, TSS, TPR, and FPR.
#' The bootstrap method resamples the data with replacement to estimate the sampling
#' distribution of each metric. Confidence intervals are added as additional columns.
#' @examples
#' # Basic usage
#' response = runif(100, 0.5, 1.5)
#' predictor = response * exp(rnorm(100, sd=0.2))
#' skillSummary(response > 1, predictor)
#' 
#' # With confidence intervals
#' skillSummary(response > 1, predictor, ci=TRUE, nBoot=1000, seed=123)
#' @export
setMethod("skillSummary", signature(response="logical", predictor="numeric"),
          function(response, predictor, reference = NULL, threshold=null, ci = FALSE, 
                   ciLevel = 0.95, nBoot = 1000, seed = NULL) {

            rocs=rocFn2(response, predictor)
            
            
            rocs<<-rocs
            
            if (is.null(reference)) {
              flag=which.max(rocs$TPR - rocs$FPR)
            } else {
              flag=which.min(abs(rocs$predictor - reference))
            }
            
            result = data.frame(
              AUC = auc_trapz(rocs$TPR, rocs$FPR),
              TSS = rocs$TPR[flag] - rocs$FPR[flag],
              ref = rocs$predictor[flag],
              TPR = rocs$TPR[flag],
              FPR = rocs$FPR[flag]
            )

            # Add confidence intervals if requested
            if (ci) {
              # For skillSummary, response and predictor are already binary/logical
              # We need to determine threshold - assume 1 if numeric, or use median
              if (is.logical(response)) {
                threshold = 0.5  # For logical, use 0.5 as threshold
                responseNumeric = as.numeric(response)
                predictorNumeric = predictor
              } else {
                threshold = median(response, na.rm = TRUE)
                responseNumeric = response
                predictorNumeric = predictor
              }
              
              ciResults = bootstrapSkillMetricsCI(responseNumeric, predictorNumeric, 
                                                  threshold = threshold,
                                                  reference = if(is.null(reference)) rocs$predictor[flag] else reference,
                                                  nBoot = nBoot,
                                                  ciLevel = ciLevel, seed = seed)
              
              # Add CI columns to result
              result$AUC_CI_lower = ciResults$ciLower[ciResults$metric == "AUC"]
              result$AUC_CI_upper = ciResults$ciUpper[ciResults$metric == "AUC"]
              result$TSS_CI_lower = ciResults$ciLower[ciResults$metric == "TSS"]
              result$TSS_CI_upper = ciResults$ciUpper[ciResults$metric == "TSS"]
              result$TPR_CI_lower = ciResults$ciLower[ciResults$metric == "TPR"]
              result$TPR_CI_upper = ciResults$ciUpper[ciResults$metric == "TPR"]
              result$FPR_CI_lower = ciResults$ciLower[ciResults$metric == "FPR"]
              result$FPR_CI_upper = ciResults$ciUpper[ciResults$metric == "FPR"]
            }
            
            return(result)
          })

setMethod("skillSummary", signature(response="numeric", predictor="numeric"),
          function(response, predictor, reference = NULL, threshold=as.numeric(1), ci = FALSE, 
                   ciLevel = 0.95, nBoot = 1000, seed = NULL) {
           
            skillSummary(response>threshold,predictor=predictor,reference=reference,threshold=threshold,
                         ci=ci,ciLevel=ciLevel,nBoot=nBoot,seed=seed)})
            

skillSummaryOld<-function(om,mp) {
  roc1 = rocFn2(om, mp)
  AUC = auc_trapz(roc1$TPR, roc1$FPR)
  TPR = roc1$TPR
  FPR = roc1$FPR
  ref = roc1$predictor
  flag = min((ref - 1)^2) == (ref - 1)^2
  flg2 = (TPR - FPR) == max(TPR - FPR)
  rtn = data.frame(AUC = AUC, TSS = ((TPR - FPR)[flag])[1], 
                   BSS = ((TPR - FPR)[flg2])[1], ref = ref[flg2][1], TPR = TPR[flag][1], 
                   FPR = FPR[flag][1], TPR2 = TPR[flg2][1], FPR2 = FPR[flg2][1])
  rtn}

cm<-function(hat,response){
  if (is.logical(hat)) hat=factor(hat,levels=c("FALSE","TRUE"))
  if (is.logical(response)) response=factor(response,levels=c("FALSE","TRUE"))
  
  # ------------------------------------------------------------------------------
  # Step 2: Compute the confusion matrix with evaluation metrics
  # ------------------------------------------------------------------------------
  CM=confusionMatrix(hat, response, mode="everything")
  
  # Overall accuracy and Cohen's kappa
  accuracy=CM$overall["Accuracy"]
  kappa   =CM$overall["Kappa"]
  #return(CM$overall)
  # ------------------------------------------------------------------------------
  # Step 3: Extract per-class metrics (precision, recall, F1-score)
  # ------------------------------------------------------------------------------
  if (is.null(dimnames(CM$byClass)[[1]])){
    TSS=CM$byClass["Sensitivity"]+CM$byClass["Specificity"]-1
    names(TSS)="TSS"
    return(c(CM$byClass,TSS))} 
  
  class=gsub("Class: ","",dimnames(CM$byClass)[[1]])
  
  transform(data.frame("class"=class,CM$byClass),TSS=unlist(Sensitivity+Specificity-1))}

skillProb<-function(stock,harvest) {
  
  b =  pmax(pmin(as.integer(stock),  1),0)
  f =1-pmax(pmin(as.integer(harvest),1),0)
  p =f*b
  collapsed=(1-b)*(1-f)
  
  red   =collapsed
  green =p
  yellow=1-p-collapsed
  
  overFished =1-b
  overFishing=1-f  
  orange     =as.numeric(!overFished&overFishing)
  
  data.frame(red=red,green=green,yellow=yellow-orange,orange=orange,
             overFished =overFished,overFishing=overFishing)}

cmKobe<-function(stock.om,harvest.om,stock.mp,harvest.mp,what=c("red","green","yellow","orange")){
  
  dt1=skillProb(stock.om,harvest.om)
  dt1$yellow=dt1$yellow-dt1$orange
  dt1=suppressMessages(melt(dt1[,what]))
  dt1=suppressWarnings(cbind(dt1,row=rep(seq(length(stock.om)),length(what))))
  dt1=subset(dt1,value==1)
  dt1=dt1[order(dt1$row),c("row","variable")]
  
  dt2=skillProb(stock.mp,harvest.mp)
  dt2$yellow=dt2$yellow-dt2$orange
  dt2=suppressMessages(subset(melt(dt2[,what])))
  dt2=suppressWarnings(cbind(dt2,row=rep(seq(length(stock.mp)),length(what))))
  dt2=subset(dt2,value==1)
  dt2=dt2[order(dt2$row),c("row","variable")]
  
  dt=merge(dt1,dt2,by=c("row"))
  names(dt)[2:3]=c("om","mp")
  cm(dt[,"om"],dt[,"mp"])}

auc_trapz2<-function(x, y) {
  sum((x[-length(x)] + x[-1]) * (y[-length(y)] + y[-1])) / (2 * diff(x) * diff(y))
}
