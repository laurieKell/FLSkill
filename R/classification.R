
#' @title Receiver Operating Characteristic (ROC) Curve Generator
#' @description Creates ROC curve data for stock status classification performance.
#' @param labels Logical vector of true stock statuses (TRUE=overfished)
#' @param scores Numeric vector of model predictions (e.g., B/BMSY ratios)
#' @return Data.frame with columns:
#' \itemize{
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item labels: Ordered true labels
#'   \item reference: Threshold values
#' }
#' @examples
#' obs=runif(100, 0.5, 1.5) < 1
#' pred=obs + rnorm(100, sd=0.3)
#' roc_data=rocFn(obs, pred)
#' @export
rocFn<-function(labels, ind) {
  ord           =order(ind, decreasing=TRUE)  
  labels_ordered=labels[ord]
  data.frame(
    TPR   =cumsum( labels_ordered)/sum( labels_ordered),
    FPR   =cumsum(!labels_ordered)/sum(!labels_ordered),
    labels=labels_ordered,
    ind   =ind[ord])}

#' @title Calculate Prediction Skill Scores
#' @description Computes threshold-based skill scores including TSS (True Skill Statistic) and AUC (Area Under the Curve) for fishery stock assessment models.
#' @param x Numeric vector of model predictions (e.g., estimated stock biomass)
#' @param y Numeric vector of observed values (e.g., survey biomass index)
#' @param reference value for status classification (default=1, typical BMSY threshold)
#' @param threshold for classification of ind, if not provided optimised, i.e. tuned
#' @return Data.frame containing optimal threshold and associated metrics:
#' \itemize{
#'   \item ref: Optimal reference value
#'   \item TP: True positives at threshold
#'   \item TN: True negatives at threshold
#'   \item FP: False positives at threshold
#'   \item FN: False negatives at threshold  
#'   \item TSS: True Skill Statistic (TPR - FPR)
#'   \item TPR: True Positive Rate (sensitivity)
#'   \item FPR: False Positive Rate (1 - specificity)
#'   \item AUC: Area Under ROC Curve
#' }
#' @examples
#' # Simulate stock assessment data
#' obsStatus=rlnorm(100, meanlog=log(1), sdlog=0.5)
#' predStatus=obsStatus * exp(rnorm(100, sd=0.3))
#' skillScore(predStatus, obsStatus > 1, reference=1)
#' @importFrom FLCore auc
#' @export
skillScore<-function(state, ind, reference = NULL, threshold = 1) {
  rocs=rocFn(state > threshold, ind)
  
  if (is.null(reference)) {
    flag=which.max(rocs$TPR - rocs$FPR)
    reference=rocs$ind[flag]
  } else {
    flag=which.min(abs(rocs$ind-reference))  # Find closest threshold
  }
  
  TP=sum(state > threshold  & ind > rocs$ind[flag])
  TN=sum(state <= threshold & ind <=rocs$ind[flag])
  FP=sum(state > threshold  & ind <=rocs$ind[flag])
  FN=sum(state <= threshold & ind > rocs$ind[flag])
  
  data.frame(
    AUC = auc(rocs$TPR, rocs$FPR),
    TSS = rocs$TPR[flag] - rocs$FPR[flag],
    ref = rocs$ind[flag],
    TPR = rocs$TPR[flag],
    FPR = rocs$FPR[flag],
    TP = TP, TN = TN, FP = FP, FN = FN)}

#' @title Stock Assessment Model Summary
#' @description Computes comprehensive performance metrics for fishery management procedures.
#' @param state Numeric vector of operating model outputs (true values)
#' @param ind Numeric vector of management procedure predictions
#' @return Data.frame containing:
#' \itemize{
#'   \item AUC: Area Under ROC Curve
#'   \item TSS: True Skill Statistic at reference threshold
#'   \item BSS: Best achievable skill score
#'   \item best: optimal reference level to get BSS
#'   \item TPR/FPR: Rates at reference threshold
#'   \item TPR2/FPR2: Rates at optimal threshold
#' }
#' @examples
#' state=runif(100, 0.5, 1.5)
#' ind=state * exp(rnorm(100, sd=0.2))
#' skillSummary(state>1, ind)
#' @export
skillSummary<-function(label, ind, reference = NULL) {
  rocs=rocFn(label, ind)
  
  if (is.null(reference)) {
    flag=which.max(rocs$TPR - rocs$FPR)
  } else {
    flag=which.min(abs(rocs$ind-reference))
  }
  
  data.frame(
    AUC = auc(rocs$TPR, rocs$FPR),
    TSS = rocs$TPR[flag] - rocs$FPR[flag],
    ref = rocs$ind[flag],
    TPR = rocs$TPR[flag],
    FPR = rocs$FPR[flag]
  )}


skillSummaryOld<-function(om,mp) {
  roc1 = rocFn(om, mp)
  AUC = with(roc1, FLCore:::auc(TPR = TPR, FPR = FPR))
  TPR = roc1$TPR
  FPR = roc1$FPR
  ref = roc1$reference
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



#' @title True Skill Statistic (TSS) Calculator
#' @description Calculates the True Skill Statistic for binary classification in stock assessments.
#' @param TP True Positives (correct overfished predictions)
#' @param TN True Negatives (correct healthy stock predictions)
#' @param FP False Positives (false overfished predictions)
#' @param FN False Negatives (false healthy stock predictions)
#' @return Numeric TSS value ranging from -1 to +1 (perfect skill)
#' @examples
#' TSS(TP=45, TN=30, FP=10, FN=15) # Good skill (0.5)
#' @export
TSS<-function(TP,TN,FP,FN) TP/(FN+TP) - TN/(FP+TN)

auc<-function(TPR, FPR) {
  dFPR=c(diff(FPR), 0)
  dTPR=c(diff(TPR), 0)
  sum(TPR*dFPR)+sum(dTPR*dFPR)/2}


#' @title Confusion Matrix Calculator
#' @description Computes confusion matrix components for stock status classification.
#' @param x Numeric vector of observed values (e.g., true B/BMSY)
#' @param y Numeric vector of predicted values
#' @return Data.frame with:
#' \itemize{
#'   \item TP: True positives (both < 1)
#'   \item TN: True negatives (both >= 1)
#'   \item FP: False positives (observed <1 but predicted >=1)
#'   \item FN: False negatives (observed >=1 but predicted <1)
#' }
#' @examples
#' obs=runif(100, 0.5, 1.5)
#' pred=obs * exp(rnorm(100, sd=0.2))
#' PN(obs < 1, pred < 1)
#' @export
PN<-function(x,y) {
  data.frame(TP=sum(x > 0 & y > 0),
             TN=sum(x <=0 & y <=0),
             FP=sum(x > 0 & y <=0),
             FN=sum(x <=0 & y > 0))}
