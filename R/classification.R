#' @title Receiver Operating Characteristic (ROC) Curve Generator
#' @description Creates ROC curve data for stock status classification performance.
#' @param obs Logical vector of true stock statuses (TRUE=overfished)
#' @param pred Numeric vector of model predictions (e.g., B/BMSY ratios)
#' @return Data.frame with columns:
#' \itemize{
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item obs: Ordered true labels
#'   \item pred: Threshold values
#' }
#' @examples
#' obs=runif(100, 0.5, 1.5) < 1
#' pred=rnorm(100)
#' roc_data=rocFn2(obs, pred)
#' @export
rocFn2<-function(obs, pred) {
  ord=order(pred, decreasing = TRUE)
  obs_ordered=obs[ord]
  data.frame(
    TPR = cumsum(obs_ordered) / sum(obs_ordered),
    FPR = cumsum(!obs_ordered) / sum(!obs_ordered),
    obs = obs_ordered,
    pred = pred[ord]
  )
}

#' @rdname skillScore
#' @export
setMethod("skillScore", signature(obs="numeric", pred="numeric"),
          function(obs, pred, reference = NULL, threshold = 1) {
            rocs=rocFn2(obs > threshold, pred)
            if (is.null(reference)) {
              flag=which.max(rocs$TPR - rocs$FPR)
              reference=rocs$pred[flag]
            } else {
              flag=which.min(abs(rocs$pred - reference))
            }
            TP=sum(obs > threshold & pred > rocs$pred[flag])
            TN=sum(obs <= threshold & pred <= rocs$pred[flag])
            FP=sum(obs > threshold & pred <= rocs$pred[flag])
            FN=sum(obs <= threshold & pred > rocs$pred[flag])
            data.frame(
              AUC = auc_trapz(rocs$TPR, rocs$FPR),
              TSS = rocs$TPR[flag] - rocs$FPR[flag],
              ref = rocs$pred[flag],
              TPR = rocs$TPR[flag],
              FPR = rocs$FPR[flag],
              TP = TP, TN = TN, FP = FP, FN = FN
            )
          })

#' @rdname skillSummary
#' @export
setMethod("skillSummary", signature(obs="numeric", pred="numeric"),
          function(obs, pred, reference = NULL) {
            rocs=rocFn2(obs, pred)
            if (is.null(reference)) {
              flag=which.max(rocs$TPR - rocs$FPR)
            } else {
              flag=which.min(abs(rocs$pred - reference))
            }
            data.frame(
              AUC = auc_trapz(rocs$TPR, rocs$FPR),
              TSS = rocs$TPR[flag] - rocs$FPR[flag],
              ref = rocs$pred[flag],
              TPR = rocs$TPR[flag],
              FPR = rocs$FPR[flag]
            )
          })

skillSummaryOld<-function(om,mp) {
  roc1 = rocFn2(om, mp)
  AUC = auc_trapz(roc1$TPR, roc1$FPR)
  TPR = roc1$TPR
  FPR = roc1$FPR
  ref = roc1$pred
  flag = min((ref - 1)^2) == (ref - 1)^2
  flg2 = (TPR - FPR) == max(TPR - FPR)
  rtn = data.frame(AUC = AUC, TSS = ((TPR - FPR)[flag])[1], 
                   BSS = ((TPR - FPR)[flg2])[1], ref = ref[flg2][1], TPR = TPR[flag][1], 
                   FPR = FPR[flag][1], TPR2 = TPR[flg2][1], FPR2 = FPR[flg2][1])
  rtn}

cm<-function(hat,obs){
  if (is.logical(hat)) hat=factor(hat,levels=c("FALSE","TRUE"))
  if (is.logical(obs)) obs=factor(obs,levels=c("FALSE","TRUE"))
  
  # ------------------------------------------------------------------------------
  # Step 2: Compute the confusion matrix with evaluation metrics
  # ------------------------------------------------------------------------------
  CM=confusionMatrix(hat, obs, mode="everything")
  
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

auc_trapz<-function(x, y) {
  sum((x[-length(x)] + x[-1]) * (y[-length(y)] + y[-1])) / (2 * diff(x) * diff(y))
}
