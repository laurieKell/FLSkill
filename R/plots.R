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

# Internal helper function for ROC calculation
fnRoc <- function(labels, ind) {
  labels = labels[order(ind, decreasing = TRUE)]
  data.frame(TPR = cumsum(labels) / sum(labels),
             FPR = cumsum(!labels) / sum(!labels),
             labels,
             reference = sort(ind))
}

# Internal helper to safely try a function call
tryIt <- function(expr) {
  tryCatch(expr, error = function(e) {
    warning("Error in calculation: ", conditionMessage(e))
    return(NULL)
  })
}

# Internal helper to calculate AUC using trapezoidal rule
calculateAuc <- function(TPR, FPR) {
  if (length(TPR) != length(FPR) || length(TPR) < 2) {
    return(NA_real_)
  }
  ord = order(FPR)
  TPR = TPR[ord]
  FPR = FPR[ord]
  sum(diff(FPR) * (head(TPR, -1) + tail(TPR, -1)) / 2)
}

# Internal helper to calculate skill metrics for a group
calculateSkillMetrics <- function(obs, pred, threshold = 1, reference = 1) {
  # Remove NAs
  valid = !is.na(obs) & !is.na(pred)
  obs = obs[valid]
  pred = pred[valid]
  
  if (length(obs) == 0 || length(pred) == 0) {
    return(data.frame(AUC = NA_real_, TSS = NA_real_, BSS = NA_real_, 
                      ref = NA_real_, TPR = NA_real_, FPR = NA_real_,
                      TPR2 = NA_real_, FPR2 = NA_real_))
  }
  
  # Create binary labels
  labels = obs > threshold
  
  # Calculate ROC curve
  ord = order(pred, decreasing = TRUE)
  labelsOrdered = labels[ord]
  roc1 = data.frame(
    TPR = cumsum(labelsOrdered) / sum(labelsOrdered),
    FPR = cumsum(!labelsOrdered) / sum(!labelsOrdered),
    labels = labelsOrdered,
    pred = pred[ord]
  )
  
  # Calculate AUC
  auc = calculateAuc(roc1$TPR, roc1$FPR)
  
  # Find TSS at reference point
  tpr = roc1$TPR
  fpr = roc1$FPR
  ref = roc1$pred
  flag = min((ref - reference)^2) == (ref - reference)^2
  idxRef = which(flag)[1]
  
  # Find best TSS (maximum TPR - FPR)
  flg2 = (tpr - fpr) == max(tpr - fpr)
  idxBest = which(flg2)[1]
  
  # Return summary
  data.frame(
    AUC = auc,
    TSS = (tpr - fpr)[idxRef],
    BSS = (tpr - fpr)[idxBest],
    ref = ref[idxBest],
    TPR = tpr[idxRef],
    FPR = fpr[idxRef],
    TPR2 = tpr[idxBest],
    FPR2 = fpr[idxBest]
  )
}

#' @rdname skillPlot
#' @param data Data.frame containing assessment results (optional if obs/pred are vectors)
#' @param obs Character name of column with true stock status, or numeric vector
#' @param pred Character name of column with predicted status, or numeric vector  
#' @param group Character name of grouping column (e.g., "Scenario"). If NULL, automatically uses "Scenario" if present, otherwise no grouping.
#' @param threshold Numeric threshold for classification (default=1)
#' @param reference Reference value for status classification (default=1)
#' @param xLabel Axis label for plots (default="")
#' @param limits X-axis limits for density plots (default=c(0,5))
#' @param labels Character vector of length 2 for observed/predicted labels (default=c("Observed", "Predicted"))
#' @param panels Character vector specifying which panels to show: "density", "scatter", "roc" (default: all three)
#' @param logScale Logical, use log scale for scatter plot (default=TRUE)
#' @param maxPoints Integer, maximum number of points to plot in scatter (default=1000)
#' @param themeStyle Character, ggplot2 theme style: "bw", "minimal", "classic" (default="bw")
#' @return ggplot object with diagnostic visualization panels
#' @examples
#' set.seed(123)
#' obs  = rlnorm(200, meanlog=log(1), sdlog=0.5)
#' df = data.frame(Scenario = rep(1:2, each=100),
#'                  obs  = obs,
#'                  pred = obs*rlnorm(200, meanlog=log(1), sdlog=0.1))
#' # Using column names with data.frame
#' skillPlot(obs="obs", pred="pred", data=df)
#' 
#' # Using numeric vectors directly
#' skillPlot(obs=df$obs, pred=df$pred)
#' 
#' # Custom panels
#' skillPlot(obs="obs", pred="pred", data=df, panels=c("scatter", "roc"))
#' 
#' # Custom grouping
#' skillPlot(obs="obs", pred="pred", data=df, group="Scenario")
#' @import ggplot2
#' @export
setMethod("skillPlot", 
          signature(obs="ANY", pred="ANY"),
          function(obs, pred, data = NULL, group = NULL, threshold = 1, reference = 1,
                   xLabel = "", limits = c(0, 5),
                   labels = c("Observed", "Predicted"),
                   panels = c("density", "scatter", "roc"),
                   logScale = TRUE, maxPoints = 1000, themeStyle = "bw") {
            
            # Handle different input types
            if (is.character(obs) && is.character(pred) && !is.null(data)) {
              # Column names provided with data.frame
              if (!obs %in% names(data) || !pred %in% names(data)) {
                stop("Column names 'obs' and 'pred' must exist in 'data'")
              }
              dat = data.frame(obs = data[[obs]], pred = data[[pred]])
              if (!is.null(group) && group %in% names(data)) {
                dat$group = data[[group]]
              } else if ("Scenario" %in% names(data)) {
                dat$group = data[["Scenario"]]
              } else {
                dat$group = "All"
              }
            } else if (is.numeric(obs) && is.numeric(pred)) {
              # Direct numeric vectors
              if (length(obs) != length(pred)) {
                stop("'obs' and 'pred' must have the same length")
              }
              dat = data.frame(obs = obs, pred = pred, group = "All")
            } else {
              stop("Invalid input: 'obs' and 'pred' must be either column names (with 'data') or numeric vectors")
            }
            
            # Remove NAs
            dat = dat[!is.na(dat$obs) & !is.na(dat$pred), , drop = FALSE]
            if (nrow(dat) == 0) {
              stop("No valid data points after removing NAs")
            }
            
            # Calculate skill metrics by group
            if ("group" %in% names(dat)) {
              groups = unique(dat$group)
              smryList = lapply(groups, function(g) {
                subsetDat = dat[dat$group == g, , drop = FALSE]
                metrics = calculateSkillMetrics(subsetDat$obs, subsetDat$pred, 
                                                   threshold = threshold, reference = reference)
                metrics$group = g
                return(metrics)
              })
              smry = do.call(rbind, smryList)
              
              # Calculate ROC data by group
              rocList = lapply(groups, function(g) {
                subsetDat = dat[dat$group == g, , drop = FALSE]
                rocDat = tryIt(fnRoc(subsetDat$obs > reference, subsetDat$pred))
                if (!is.null(rocDat)) {
                  rocDat$group = g
                  return(rocDat)
                }
                return(NULL)
              })
              rocDat = do.call(rbind, rocList[!sapply(rocList, is.null)])
              
              # Facet formula
              facetFormula = if (length(groups) > 1) "group ~ ." else NULL
            } else {
              smry = calculateSkillMetrics(dat$obs, dat$pred, threshold = threshold, reference = reference)
              smry$group = "All"
              rocDat = tryIt(fnRoc(dat$obs > reference, dat$pred))
              if (!is.null(rocDat)) {
                rocDat$group = "All"
              }
              facetFormula = NULL
            }
            
            # Select theme
            themeFunc = switch(themeStyle,
                               "bw" = theme_bw,
                               "minimal" = theme_minimal,
                               "classic" = theme_classic,
                               theme_bw)
            
            plotList = list()
            
            # Panel 1: Density plot
            if ("density" %in% panels) {
              # Reshape data for density plot
              dt2 = data.frame(
                value = c(dat$obs, dat$pred),
                variable = factor(rep(c("obs", "pred"), each = nrow(dat)),
                                 levels = c("obs", "pred"),
                                 labels = labels)
              )
              if ("group" %in% names(dat)) {
                dt2$group = rep(dat$group, 2)
              }
              
              p1 = ggplot(dt2, aes(x = value, y = variable, fill = variable))
              
              # Use ggridges if available, otherwise use geom_density
              if (requireNamespace("ggridges", quietly = TRUE)) {
                p1 = p1 + ggridges::geom_density_ridges(alpha = 0.5, scale = 1.2, rel_min_height = 0.01)
              } else {
                # Fallback to regular density plots
                p1 = p1 + geom_density(alpha = 0.5, position = "identity") +
                  coord_flip()
              }
              
              if (!is.null(facetFormula)) {
                p1 = p1 + facet_grid(as.formula(facetFormula), scales = "free")
              }
              
              p1 = p1 +
                geom_vline(xintercept = reference, color = "red") +
                scale_y_discrete(expand = c(0, 0)) +
                themeFunc() +
                theme(legend.position = "none",
                      plot.title = element_text(hjust = 0),
                      strip.text = element_text(angle = 0)) +
                labs(title = "Distribution", x = xLabel, y = "")
              
              if (nrow(smry) > 0 && "ref" %in% names(smry)) {
                p1 = p1 + geom_vline(aes(xintercept = ref), data = smry, col = "blue")
              }
              
              plotList[["density"]] = p1
            }
            
            # Panel 2: Scatter plot
            if ("scatter" %in% panels) {
              # Sample points if too many
              nSample = min(nrow(dat), maxPoints)
              if (nSample < nrow(dat)) {
                datSample = dat[sample(nrow(dat), nSample), , drop = FALSE]
              } else {
                datSample = dat
              }
              
              p2 = ggplot(datSample, aes(x = obs, y = pred)) +
                geom_point(size = 1.25, alpha = 0.75)
              
              if (!is.null(facetFormula)) {
                p2 = p2 + facet_grid(as.formula(facetFormula))
              }
              
              p2 = p2 +
                geom_vline(xintercept = reference) +
                geom_hline(yintercept = reference, col = "red")
              
              if (nrow(smry) > 0 && "ref" %in% names(smry)) {
                p2 = p2 + geom_hline(aes(yintercept = ref), data = smry, col = "blue")
              }
              
              if (logScale) {
                p2 = p2 + scale_x_log10() + scale_y_log10()
              }
              
              # Add confusion matrix labels
              if (nrow(dat) > 0) {
                obsRange = range(dat$obs, na.rm = TRUE)
                predRange = range(dat$pred, na.rm = TRUE)
                if (logScale) {
                  xPos = 10^(log10(obsRange))
                  yPos = 10^(log10(predRange))
                } else {
                  xPos = obsRange
                  yPos = predRange
                }
                
                labelData = data.frame(
                  x = c(mean(xPos), xPos[1], mean(xPos), xPos[1]),
                  y = c(mean(yPos), mean(yPos), yPos[1], yPos[1]),
                  label = c("TP", "FP", "FN", "TN"),
                  hjust = c(0.5, 0, 0.5, 0),
                  vjust = c(0.5, 0.5, -0.75, -0.75)
                )
                p2 = p2 + geom_label(aes(x = x, y = y, label = label, 
                                         hjust = hjust, vjust = vjust),
                                     data = labelData, size = 4, inherit.aes = FALSE)
              }
              
              # Add metric labels
              if (nrow(smry) > 0) {
                if (logScale) {
                  xMax = 10^(log10(max(dat$obs, na.rm = TRUE)))
                  yMax = 10^(log10(max(dat$pred, na.rm = TRUE)))
                } else {
                  xMax = max(dat$obs, na.rm = TRUE)
                  yMax = max(dat$pred, na.rm = TRUE)
                }
                
                p2 = p2 +
                  geom_label(aes(x = Inf, y = ref, label = paste("BSS=", round(BSS, 2))),
                            data = smry, fill = "white", hjust = 1, vjust = 0,
                            size = 4.0, col = "blue", alpha = 0.9, inherit.aes = FALSE) +
                  geom_label(aes(x = limits[1], y = reference, 
                                label = paste("TSS=", round(TSS, 2))),
                            data = smry, fill = "white", hjust = 0, vjust = 0,
                            size = 4.0, col = "red", alpha = 0.9, inherit.aes = FALSE)
              }
              
              p2 = p2 +
                themeFunc() +
                theme(legend.position = "none",
                      plot.title = element_text(hjust = 0)) +
                labs(title = "Confusion Matrix",
                     x = "Operating Model",
                     y = "Indicator")
              
              plotList[["scatter"]] = p2
            }
            
            # Panel 3: ROC curve
            if ("roc" %in% panels && !is.null(rocDat) && nrow(rocDat) > 0) {
              p3 = ggplot(rocDat, aes(x = FPR, y = TPR)) +
                geom_path(alpha = 0.5)
              
              if (!is.null(facetFormula)) {
                p3 = p3 + facet_grid(as.formula(facetFormula))
              }
              
              if (nrow(smry) > 0) {
                p3 = p3 +
                  geom_point(aes(x = FPR, y = TPR), data = smry, col = "red", size = 3, inherit.aes = FALSE) +
                  geom_point(aes(x = FPR2, y = TPR2), data = smry, col = "blue", size = 3, inherit.aes = FALSE) +
                  geom_label(aes(x = 1.0, y = 0, label = paste("AUC=", round(AUC, 2))),
                            data = smry, fill = "white", hjust = 1, vjust = 0, alpha = 0.9,
                            size = 4, inherit.aes = FALSE)
              }
              
              p3 = p3 +
                geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = 1.0, col = "grey74") +
                themeFunc() +
                theme(legend.position = "none",
                      plot.title = element_text(hjust = 0)) +
                labs(title = "ROC Curve",
                     x = "FPR (1-Specificity)",
                     y = "TPR (Sensitivity)")
              
              plotList[["roc"]] = p3
            }
            
            # Combine plots
            if (length(plotList) == 0) {
              stop("No panels selected. Choose at least one panel from: density, scatter, roc")
            }
            
            if (length(plotList) == 1) {
              return(plotList[[1]])
            }
            
            # Use ggpubr if available, otherwise grid.arrange
            if (requireNamespace("ggpubr", quietly = TRUE)) {
              combined = do.call(ggpubr::ggarrange, c(plotList, 
                                                      list(ncol = length(plotList),
                                                           widths = rep(1, length(plotList)),
                                                           common.legend = TRUE,
                                                           legend = "none",
                                                           align = "h")))
            } else if (requireNamespace("gridExtra", quietly = TRUE)) {
              combined = do.call(gridExtra::grid.arrange, c(plotList, ncol = length(plotList)))
            } else {
              # Fallback: return list of plots
              warning("Neither 'ggpubr' nor 'gridExtra' available. Returning list of plots.")
              return(plotList)
            }
            
            return(combined)
          })

# Backward-compatible wrapper for old signature: skillPlot(data, obs="col1", hat="col2")
# Note: This is not exported - use the new signature: skillPlot(obs="col1", pred="col2", data=data)
#' @rdname skillPlot  
#' @param hat Character name of column with predicted status (old parameter name, use 'pred' in new signature)
#' @keywords internal
skillPlot_old <- function(data, obs, hat, threshold = 1, reference = 1,
                          xLabel = "", limits = c(0, 5),
                          labels = c(obs, hat), group = NULL, ...) {
  # Map old signature to new generic
  skillPlot(obs = obs, pred = hat, data = data, group = group,
            threshold = threshold, reference = reference,
            xLabel = xLabel, limits = limits, labels = labels, ...)
}

# Note: The S4 method handles new signatures:
# - skillPlot(obs=data$col1, pred=data$col2) 
# - skillPlot(obs="col1", pred="col2", data=data)
# For old signature skillPlot(data, obs="col1", hat="col2"), 
# users should use: skillPlot(obs="col1", pred="col2", data=data)

#'
#' @examples
#' \dontrun{
#' library(remotes)
#' remotes::install_github('flr/FLCore', ref='devel')
#' library(FLCore)
#' library(ggplotFL)
#' library(FLSkill)
#' library(plyr)
#' library(reshape)
#' library(ggridges)
#' library(ggpubr)
#' library(pROC)
#' 
#' testDF=data.frame(OM       =seq(0,2,length.out=101),
#'                   Indicator=seq(0,2,length.out=101)+rnorm(101,0,0.3),
#'                   Scenario ="Reference",stringsAsFactors=TRUE)
#' 
#' ggplot(testDF)+
#'   geom_point(aes(OM,Indicator))+
#'   xlab("True Values")+ylab("Indicator")
#' 
#' skillPlot(testDF,obs="OM",hat="Indicator")
#' 
#' pROC::auc(testDF$OM>1,testDF$Indicator)
#' }


#' @title Taylor Diagram Generator
#' @description Creates Taylor diagrams for visual model skill assessment in stock assessments.
#' @param minR Minimum reference value (default=0.25)
#' @param maxR Maximum reference value (default=1.75)
#' @param contours Number of contour lines (default=7)
#' @param nLines Number of angular lines (default=10)
#' @param x0 Central reference point (default=1)
#' @param refRMin Minimum reference circle radius (default=0.25)
#' @param refRMax Maximum reference circle radius (default=2)
#' @param refContours Number of reference circles (default=8)
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
taylorDiagram <- function(minR = 0.25, maxR = 1.75, contours = 7, 
                        nLines = 10, x0 = 1, refRMin = 0.25,
                        refRMax = 2, refContours = 8, full = FALSE) {
  # Create base plot structure
  p = ggplot() +
    theme_minimal() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Standard Deviation", y = "sigma")
  
  # Add correlation contours
  angles = seq(0, ifelse(full, pi, pi/2), length.out = nLines)
  radii = seq(minR, maxR, length.out = contours)
  
  # Add reference circles
  for(r in radii) {
    circleData = data.frame(
      x = r * cos(angles),
      y = r * sin(angles)
    )
    p = p + geom_path(data = circleData, aes(x, y), 
                    linetype = "dashed", color = "gray70")
  }
  
  return(p)
}
