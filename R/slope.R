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
             
             # Build dimnames from output - this ensures correct structure
             years_out = sort(unique(df_out$year))
             dim_list = list(year = as.character(years_out))
             
             # Add other dimensions from output
             if (length(facs) > 0) {
               for (fac in facs) {
                 dim_list[[fac]] = as.character(sort(unique(df_out[[fac]])))
               }
             }
             
             # Get other dimensions from original object (quant, unit, season, area, iter)
             orig_dims = dimnames(object)
             for (dim_name in c("quant", "unit", "season", "area", "iter")) {
               if (!is.null(orig_dims[[dim_name]]) && length(orig_dims[[dim_name]]) > 0) {
                 dim_list[[dim_name]] = orig_dims[[dim_name]]
               }
             }
             
             # Create FLQuant with correct dimensions
             rtnFlq = FLQuant(NA, dimnames = dim_list)
             
             # Fill values by matching rows in data.frame to FLQuant positions
             # Use a simpler approach: convert to array, fill, convert back
             dim_names = names(dim_list)
             dims_vec = sapply(dim_list, length)
             
             # Convert FLQuant to array for easier indexing
             arr = array(rtnFlq@.Data, dim = dims_vec, dimnames = dim_list)
             
             for (i in seq_len(nrow(df_out))) {
               year_char = as.character(df_out$year[i])
               
               if (length(facs) == 0) {
                 # Simple case: just year
                 year_idx = which(dim_list$year == year_char)
                 arr[year_idx] = df_out$data[i]
               } else {
                 # Multiple dimensions - build index list in correct order
                 idx_list = list()
                 
                 # Build index in the order of dim_names
                 for (dim_name in dim_names) {
                   if (dim_name == "year") {
                     idx_list[[dim_name]] = which(dim_list$year == year_char)
                   } else if (dim_name %in% facs) {
                     fac_val = as.character(df_out[[dim_name]][i])
                     idx_list[[dim_name]] = which(dim_list[[dim_name]] == fac_val)
                   } else {
                     # Other dimensions - use first element
                     idx_list[[dim_name]] = 1
                   }
                 }
                 
                 # Use do.call to index the array
                 arr[do.call("[", c(list(arr), idx_list))] = df_out$data[i]
               }
             }
             
             # Convert back to FLQuant
             rtnFlq = FLQuant(arr)
             
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
