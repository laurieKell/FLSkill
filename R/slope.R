#' @importFrom data.table data.table setkey

#' @title Calculate Rolling Slope
#' @description Calculates rolling linear regression slopes for time series data.
#' For each position in the time series, computes the slope of a linear regression
#' fitted to the previous n points (including the current point).
#'
#' @param object FLQuant or numeric vector. The time series data for which to calculate slopes.
#' @param n Integer (default=3). The window size for calculating slopes. Must be at least 2.
#' @param ... Additional arguments (currently not used).
#'
#' @return 
#' \itemize{
#'   \item For FLQuant: Returns an FLQuant object with slopes calculated for each year,
#'         starting from year n. The result has units "slope" and maintains the same
#'         structure (seasons, iterations, etc.) as the input.
#'   \item For numeric: Returns a numeric vector of slopes, with length = length(object) - n + 1.
#' }
#'
#' @details
#' The function calculates rolling slopes by fitting linear regressions to sliding windows
#' of size n. For an FLQuant object, slopes are calculated separately for each combination
#' of other dimensions (season, area, iter, etc.). The slope represents the rate of change
#' per unit time (year).
#'
#' @examples
#' \dontrun{
#' # Example with FLQuant
#' library(FLCore)
#' flq = FLQuant(rlnorm(400), dimnames = list(year = 1:10, season = 1:4, iter = 1:10))
#' slopes = slope(flq, n = 3)
#'
#' # Example with numeric vector
#' x = c(1, 2, 3, 5, 7, 9, 11, 13)
#' slopes = slope(x, n = 3)
#' # Returns slopes for windows: [1,2,3], [2,3,5], [3,5,7], [5,7,9], [7,9,11], [9,11,13]
#' }
#'
#' @export
#' @importFrom FLCore FLQuant dims as.FLQuant
#' @importFrom stats coef lm
setGeneric("slope", function(object, ...) standardGeneric("slope"))

#' @rdname slope
#' @export
setMethod("slope", signature(object="FLQuant"),
          function(object, n=3) {
            d = dims(object)
            ny= d$maxyear - d$minyear + 1
            
            if (ny < n)
              stop("'n' longer than number of years")
            
            dat= as.data.frame(object)
            dt = data.table(dat)
            
            setkey(dt, year)
            
            facs = names(dt)[!names(dt) %in% c("year", "data")]
            
            dt_out = dt[, {
              .sd = .SD[order(year)]
              y   = .sd$year
              z   = .sd$data
              k   = length(y)
              
              if (k < n) 
                return(data.table())
  
              
              idxEnd = n:k
              slopes = vapply(idxEnd, function(i) {
                ii = (i - n + 1):i
                coef(lm(z[ii] ~ y[ii]))[[2]]
              }, numeric(1))
              
              data.table(year = y[idxEnd], data = slopes)
            }, by = facs]
            
            # Convert to data.frame
            df_out = as.data.frame(dt_out)
            
            # Ensure year is numeric (not factor)
            if (is.factor(df_out$year)) {
              df_out$year = as.numeric(levels(df_out$year))[df_out$year]
            } else {
              df_out$year = as.numeric(df_out$year)
            }
            
            # Build dimnames from output data
            years_out = sort(unique(df_out$year))
            dim_list = list(year = as.character(years_out))
            
            if (length(facs) > 0) {
              for (fac in facs) {
                dim_list[[fac]] = as.character(sort(unique(df_out[[fac]])))
              }
            }
            
            # Get other dimensions from original object (quant, unit, season, area, iter)
            # These should remain the same as input
            orig_dims = dimnames(object)
            for (dim_name in c("quant", "unit", "season", "area", "iter")) {
              if (!is.null(orig_dims[[dim_name]]) && length(orig_dims[[dim_name]]) > 0) {
                dim_list[[dim_name]] = orig_dims[[dim_name]]
              }
            }
            
            # Create FLQuant with all dimensions properly set
            rtnFlq = FLQuant(NA, dimnames = dim_list)
            
            # Fill values by matching dimension values
            for (i in seq_len(nrow(df_out))) {
              # Build index for this row - use character names for dimensions
              year_char = as.character(df_out$year[i])
              
              if (length(facs) == 0) {
                # Simple case: just year dimension
                rtnFlq[, year = year_char] = df_out$data[i]
              } else {
                # Multiple dimensions - build index list
                idx_call = list(rtnFlq, year = year_char)
                for (fac in facs) {
                  idx_call[[fac]] = as.character(df_out[[fac]][i])
                }
                # Set other dimensions to first value if not specified
                for (dim_name in c("quant", "unit", "season", "area", "iter")) {
                  if (!is.null(dim_list[[dim_name]]) && !dim_name %in% names(idx_call)) {
                    idx_call[[dim_name]] = dim_list[[dim_name]][1]
                  }
                }
                idx_call$value = df_out$data[i]
                rtnFlq = do.call("[<-", idx_call)
              }
            }
            
            units(rtnFlq) = "slope"
            
            rtnFlq})

#' @rdname slope
#' @export
setMethod("slope", signature(object = "numeric"),
          function(object, n = 3) {
            
            year = seq_along(object)
            
            k = length(year)
            if (k < n)
              stop("'n' longer than length(object)")
            
            idxEnd = n:k
            slopes = vapply(idxEnd, function(i) {
              ii = (i - n + 1):i
              coef(lm(object[ii] ~ year[ii]))[[2]]
            }, numeric(1))
            
            slopes})
