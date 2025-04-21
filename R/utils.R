
#dat=data.frame(om=rep(c(1,2),10),mp=rlnorm(20))

#aucTss(dat$om>1,dat$mp)

#' @title Trend Agreement Metrics
#' @description Calculates correlation measures between observed and predicted stock trends.
#' @param obs Numeric vector of observed time series (e.g., biomass index)
#' @param hat Numeric vector of predicted time series
#' @return Data.frame containing:
#' \itemize{
#'   \item pearson: Pearson correlation coefficient
#'   \item spearman: Spearman's rank correlation  
#'   \item direction: Proportion of matching inter-annual change directions
#' }
#' @examples
#' obs_trend=cumsum(rnorm(20))
#' pred_trend=obs_trend + rnorm(20, sd=0.5)
#' trend(obs_trend, pred_trend)
#' @export
trend<-function(obs,hat){ 
  data.frame(
    pearson =cor(hat, obs, method="pearson"),
    spearman=cor(hat, obs, method="spearman"),
    direction=mean(sign(diff(hat))== sign(diff(obs)), na.rm=TRUE))}


#' @title Stock Status Classification Metrics
#' @description Evaluates performance of stock status classification (Overfished/Healthy).
#' @param obs Numeric vector of observed stock status values
#' @param hat Numeric vector of predicted status values
#' @return Data.frame containing:
#' \itemize{
#'   \item accuracy: Overall classification accuracy
#'   \item precision: Precision for Overfished classification
#'   \item recall: Recall for Overfished classification
#' }
#' @note Status determined using threshold at 1 (B/BMSY). Check variable names in function code.
#' @examples
#' obs_status=runif(100, 0.5, 1.5)
#' pred_status=obs_status * exp(rnorm(100, sd=0.2))
#' state(obs_status, pred_status)
#' @export
state<-function(obs,hat){
  
  obs=ifelse(obs < 1, "Overfished", "Healthy")
  pred=ifelse(hat < 1, "Overfished", "Healthy")
  
  data.frame(
    accuracy =mean(obs_status==pred_status),
    precision=sum( obs=="Overfished" & pred=="Overfished")/sum(pred=="Overfished"),
    recall   =sum( obs=="Overfished" & pred=="Overfished")/sum(obs=="Overfished"))}


#' @title Variability Comparison Metrics
#' @description Compares variability characteristics between observed and predicted time series.
#' @param obs Numeric vector of observed values
#' @param hat Numeric vector of predicted values  
#' @return Data.frame containing variability ratios:
#' \itemize{
#'   \item sd: Standard deviation ratio (pred/obs)
#'   \item iqr: Interquartile range ratio
#'   \item cv: Coefficient of variation ratio
#' }
#' @examples 
#' obs=rlnorm(100, meanlog=log(1), sdlog=0.4)
#' hat=obs * exp(rnorm(100, sd=0.1))
#' variability(obs, hat)
#' @export
variability<-function(obs,hat){
  data.frame(sd=sd(hat)/sd(obs),
             iqr=IQR(hat)/IQR(obs),
             cv=(sd(hat)/mean(hat))/(sd(obs)/mean(obs)))}


#' @title Comprehensive Diagnostic Evaluation
#' @description Integrates multiple performance metrics for stock assessment model validation.
#' @param obs Numeric vector of observed values
#' @param hat Numeric vector of predicted values
#' @param ndemb Embedding dimension for permutation entropy calculation (default=5)
#' @return Data.frame containing 10 diagnostic metrics. Final classification currently non-functional.
#' @note Requires helper functions: stdz(), permutation_entropy(), ordinal_pattern_distribution()
#' @examples
#' obs=runif(100, 0.8, 1.2)
#' hat=obs * exp(rnorm(100, sd=0.15))
#' diagnostics(obs, hat)
#' @export
diagnostics<-function(obs,hat,ndemb=5){
  
  roc=rocFn(stdz(obs)>1,stdz(hat))
  tss=skillScore(stdz(hat),stdz(obs)-1)
  
  return(
    data.frame(
      trend     =cor(hat, obs),
      status    =mean((hat<1)== (obs<1)),
      sd.hat    =sd(hat),
      sd.obs    =sd(obs),
      variability=sd(hat)/sd(obs),
      auc       =FLCore:::auc(TPR=roc$TPR,FPR=roc$FPR),
      tss       =tss$TSS,
      fpr       =tss$FPR,
      tpr       =tss$TPR,
      entropy   =permutation_entropy(ordinal_pattern_distribution(obs, ndemb=ndemb))))
  
  case_when(
    trend > 0.7 & status > 0.8 & between(variability,0.8,1.2) ~ "Excellent",
    trend > 0.5 & status > 0.7 & between(variability,0.6,1.4) ~ "Adequate",
    TRUE ~ "Needs Improvement")}


#' @title Time Series Comparison Metrics
#' @description Calculates similarity measures between two stock assessment time series.
#' @param ts1 Numeric vector (e.g., observed biomass index)
#' @param ts2 Numeric vector (e.g., model-predicted biomass)
#' @return Data.frame with:
#' \itemize{
#'   \item rmse: Root Mean Square Error
#'   \item correlation: Pearson correlation
#'   \item sd: Standard deviation of residuals
#' }
#' @examples
#' obs=cumsum(rnorm(20))
#' pred=obs + rnorm(20, sd=0.5)
#' compareTS(obs, pred)
#' @export
compareTS<-function(ts1, ts2) {
  
  # Ensure same length
  min_length=min(length(ts1), length(ts2))
  ts1=ts1[1:min_length]
  ts2=ts2[1:min_length]
  
  # Basic statistics
  rmse=sqrt(mean((ts1 - ts2)^2))
  correlation=cor(ts1, ts2)
  
  # Return results as list
  results=data.frame(rmse      =rmse,
                     correlation=correlation,
                     sd        =var(ts2-ts1)^0.5)
  
  return(results)}

#' @title Optimal Lag Finder
#' @description Identifies lag with maximum cross-correlation between stock assessment time series.
#' @param obs Observed time series (e.g., survey index)
#' @param hat Predicted time series (e.g., model output)
#' @param lag.max Maximum lag to consider (default=5)
#' @return Data.frame with optimal lag and corresponding ACF value
#' @examples
#' obs=sin(seq(0, 2*pi, length=50)) + rnorm(50)
#' hat=lag(obs, 2) + rnorm(50, sd=0.2)
#' ccfFn(obs, hat)
#' @export
ccfFn<-function(obs, hat, lag.max=5){
  rtn=ccf(obs,hat,plot=FALSE,lag.max=lag.max)
  subset(data.frame(lag=rtn$lag,
                    acf=rtn$acf),acf==max(acf))}


