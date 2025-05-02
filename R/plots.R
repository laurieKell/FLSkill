# Create background rectangles using geom_rect
tssCat=data.frame(
  xmin=rep(0,5),
  xmax=rep(3,5),
  ymin=c(-Inf, 0.0, 0.2, 0.4, 0.6),
  ymax=c( 0.2, 0.2, 0.4, 0.6, Inf),
  quality=factor(       c("Negative","None","Low","Moderate","High"),
                 levels=c("Negative","None","Low","Moderate","High")))

tssCol=c("Negative"="#ffaaaa",
         "None"    ="#ffcccc", "Low" ="#ffe6cc", 
         "Moderate"="#e6ffcc", "High"="#ccffe6")

aucCat=data.frame( 
  xmin=rep(-Inf, 4),
  xmax=rep( Inf, 4),
  ymin=c(-Inf,0.6, 0.7, 0.9),
  ymax=c( 0.6,0.7, 0.9, Inf),
  quality=factor(       c("Fail","Fair","Good","Excellent"),
                 levels=c("Fail","Fair","Good","Excellent")))

aucCol=c("Fail"="#ffcccc", "Fair"     ="#ffe6cc", 
         "Good"="#e6ffcc", "Excellent"="#ccffe6")

fnTss<-function(x) {
  chk = unique(c(tssCat$ymin, tssCat$ymax))
  sapply(x, function(val) {
    tssCol[max(seq(length(chk))[val > chk])]
  })}

fnAuc<-function(x) {
  chk = unique(c(aucCat$ymin, aucCat$ymax))
  sapply(x, function(val) {
    aucCol[max(seq(length(chk))[val > chk])]
  })}

aucTss<-function(om,mp){
  rtn=FLCandy:::roc2(om,mp) 
  
  data.frame("tss"=data.frame(with(rtn[abs(rtn$reference-1)==min(abs(rtn$reference-1)),],TPR-FPR))[1,],
             data.frame("auc"=pROC:::auc(as.character(rtn$label),rtn$reference)))}



# Internal function to compute AUC using a trapezoidal rule
computeAUC<-function(TPR, FPR) {
  ord=order(FPR)
  TPR=TPR[ord]
  FPR=FPR[ord]
  # Standard trapezoidal integration
  sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)}

#' @title Stock Assessment Skill Visualizer
#' @description Generates diagnostic plots for management procedure evaluation.
#' @param x Data.frame containing assessment results
#' @param state Character name of column with true stock status
#' @param ind Character name of column with predicted status
#' @param xLabel Axis label for plots (default="")
#' @param limits X-axis limits for density plots (default=c(0,5))
#' @return ggplot object with 3-panel visualization:
#' \itemize{
#'   \item a) Density plots of observed vs predicted status
#'   \item b) Scatterplot with reference lines
#'   \item c) ROC curve with AUC
#' }
#' @examples
#' \dontrun{
#' data(assessment_data)
#' skillPlot(assessment_data, "true_status", "predicted_status")
#' }
#' @import ggplot2
#' @export
skillPlot<-function(object, state, ind, threshold=1, reference=1, xLabel="", limits=c(0,5)) {
  
  dat=transform(object,
                state=eval(sym(state)),
                ind  =eval(sym(ind)))
  
  dat=subset(   dat, !is.na(state)&!is.na(ind))
  dat=transform(dat, ratio=(ind-state)/state)
  
  smry=ddply(cbind(dat,threshold=threshold), .(Scenario), with, {
    labels=state>threshold
    roc1={ord           =order(ind, decreasing=TRUE)  
          labels_ordered=labels[ord]
          data.frame(
            TPR   =cumsum( labels_ordered)/sum( labels_ordered),
            FPR   =cumsum(!labels_ordered)/sum(!labels_ordered),
            labels=labels_ordered,
            ind   =ind[ord])}
    AUC=with(roc1, { ord=order(FPR)
                     TPR=TPR[ord]
                     FPR=FPR[ord]
                     # Standard trapezoidal integration
                     sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)})
    TPR=roc1$TPR
    FPR=roc1$FPR
    ref=roc1$ind
    flag=min((ref-1)^2)==(ref-1)^2
    flg2=(TPR-FPR)==max(TPR-FPR)
    
    rtn=data.frame(AUC=AUC,
                   TSS=((TPR-FPR)[flag])[1],
                   BSS=((TPR-FPR)[flg2])[1],
                   ref=ref[flg2][1],
                   TPR=((TPR)[flag])[1],
                   FPR=((TPR)[flag])[1],
                   TPR2=TPR[flg2][1],
                   FPR2=FPR[flg2][1])
    rtn})
  
  rocDat=ddply(dat, .(Scenario), with, FLCandy:::tryIt(rocFn(state>1,ind)))
  
  # ggridges plot
  dt2=melt(dat,c("Scenario"),c("state","ind"))
  dt2$variable=factor(dt2$variable,levels=c("state","ind"),
                      labels=c("Operating\nModel","Indicator"))
  p1=ggplot(dt2, aes(x=value, y=variable, fill=variable)) +
    geom_density_ridges(alpha=0.5, scale=1.2, rel_min_height=0.01) +
    facet_grid(Scenario~., scales="free") +
    geom_vline(xintercept=1, color="red") +
    geom_vline(aes(xintercept=ref),data=smry,col="blue") +
    scale_y_discrete(expand=c(0, 0)) +
    #scale_x_continuous(limits=limits) +
    #geom_label(aes(label=paste("TSS=",round(TSS, 2))), fill="white", #fnTss(TSS),
    #           x=Inf, y=-Inf,
    #           hjust=1, vjust=0,
    #           size=4,data=smry)+
    theme_bw()+
    theme(legend.position="none",
          plot.title=element_text(hjust=0),
          strip.text.x=element_text(angle=0))+
    labs(title = "Distribution",
         x="", 
         y="")
  
  
  # Predictions plot
  p2=ggplot(dat[sample(seq(dim(dat)[1]),pmin(dim(dat)[1],1000)),])+
    facet_grid(Scenario~.)+
    geom_point(aes(state,ind), size=1.25, alpha=0.75)+
    geom_vline(xintercept=1)+
    geom_hline(aes(yintercept=1), col="red")+
    geom_hline(aes(yintercept=ref),data=smry,col="blue")+
    #scale_x_continuous(limits=limits)+
    #scale_y_continuous(limits=limits)+
    geom_label(aes(x=Inf, y=ref,
                   label=paste("BSS=",round(BSS,2))),fill="white",#fnTss(BSS)), 
               hjust=1, vjust=0,
               size=4.0, col="blue",
               data=smry)+
    geom_label(aes(x=limits[1], y=1,
                   label=paste("TSS=",round(TSS,2))),fill="white",#fnTss(TSS)), 
               hjust=0, vjust=0, 
               size=4.0, col="red",
               data=smry)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+
    theme(legend.position="none",
          plot.title=element_text(hjust=0))+
    geom_label(aes(x=10^(log10(max(dat$state, na.rm = TRUE))), 
                   y=10^(log10(max(dat$ind,   na.rm = TRUE)))),label="TP",hjust=0.5,vjust= 0.5,size=4)+
    geom_label(aes(x=10^(log10(min(dat$state, na.rm = TRUE))), 
                   y=10^(log10(max(dat$ind,   na.rm = TRUE)))),label="FP",hjust=0,  vjust= 0.5,size=4)+
    geom_label(aes(x=10^(log10(max(dat$state, na.rm = TRUE))), 
                   y=10^(log10(min(dat$ind,   na.rm = TRUE)))),label="FN",hjust=0.5,vjust=-0.75,size=4)+
    geom_label(aes(x=10^(log10(min(dat$state, na.rm = TRUE))), 
                   y=10^(log10(min(dat$ind,   na.rm = TRUE)))),label="TN",hjust=0,  vjust=-0.75,size=4)+
    labs(title = "Confusion Matrix",
         x="Operating Model", 
         y="Indicator")
  
  p3=ggplot(rocDat)+
    facet_grid(Scenario~.)+
    geom_path( aes(FPR, TPR), alpha=0.5)+
    geom_point(aes(FPR, TPR), data=smry,col="red",  size=3)+
    geom_point(aes(FPR2,TPR2),data=smry,col="blue", size=3)+
    geom_abline(intercept=0, slope=1, linetype=2, linewidth=1.0, col="grey74")+
    geom_label(data=smry,
               aes(label=paste("AUC=",round(AUC, 2))),
               fill="white",#fnAuc(AUC),
               x   =1.0, y   =0,
               hjust=1,   vjust=0,
               size=4)+
    labs(title="ROC Curve",
         x="FPR (1-Specificity)",
         y="TPR (Sensitivity)")+
    theme_bw()+
    theme(legend.position="none",
          plot.title=element_text(hjust=0))
  
  
  # Modify your plots
  p1=p1+FLCandy:::theme_no_title
  p2=p2+FLCandy:::theme_no_title
  p3=p3
  
  # Combine plots with minimal spacing
  combined=ggarrange(p1,p2,p3,
                     ncol=3, 
                     widths=c(1.25, 1, 1),
                     common.legend=TRUE,
                     legend="none",
                     align="h")
  
  #combined=annotate_figure(combined,
  #                         top=text_grob("Assessment Performance Metrics", 
  #                                       face="bold", size=14))
  
  return(combined)}  


#' @title Taylor Diagram Generator
#' @description Creates Taylor diagrams for visual model skill assessment in stock assessments.
#' @param min_R Minimum reference value (default=0.25)
#' @param max_R Maximum reference value (default=1.75)
#' @param contours Number of contour lines (default=7)
#' @param n_lines Number of angular lines (default=10)
#' @param x_0 Central reference point (default=1)
#' @param ref_r_min Minimum reference circle radius (default=0.25)
#' @param ref_r_max Maximum reference circle radius (default=2)
#' @param ref_contours Number of reference circles (default=8)
#' @param full Display full circle (TRUE) or quadrant (FALSE) (default=FALSE)
#' @return ggplot object showing:
#' \itemize{
#'   \item Standard deviation ratios
#'   \item Correlation coefficients
#'   \item RMS differences
#' }
#' @examples
#' taylorDiagram()
#' @import ggplot2
#' @export
taylorDiagram<-function(min_R=0.25, max_R=1.75, contours=7, 
                        n_lines=10, x_0=1, ref_r_min=0.25,
                        ref_r_max=2, ref_contours=8, full=FALSE) {
  # Create base plot structure
  p=ggplot()+
    theme_minimal()+
    coord_equal()+
    scale_x_continuous(expand=c(0, 0))+
    scale_y_continuous(expand=c(0, 0))+
    labs(x="Standard Deviation", y="sigma")
  
  # Add correlation contours
  angles=seq(0, ifelse(full, pi, pi/2), length.out=n_lines)
  radii=seq(min_R, max_R, length.out=contours)
  
  # Add reference circles
  for(r in radii) {
    circle_data=data.frame(
      x=r * cos(angles),
      y=r * sin(angles)
    )
    p=p + geom_path(data=circle_data, aes(x, y), 
                    linetype="dashed", color="gray70")
  }
  
  return(p)}
