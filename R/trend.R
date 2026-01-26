library(data.table)

# Note: The generic for trend is defined in R/generics.R with signature (response, predictor)
# These methods below use a different signature (object) and conflict with the main trend generic.
# They have been commented out to resolve the signature conflict.
# If these single-object trend methods are needed, they should be renamed to a different function
# (e.g., trendSlope or calculateTrend) to avoid the naming conflict.

# setMethod("trend", signature(object="FLQuant"),
#           function(object, n=3) {
#             d = dims(object)
#             ny = d$maxyear - d$minyear + 1
#             
#             if (ny < n)
#               stop("'n' longer than number of years")
#             
#             dat = as.data.frame(object)
#             dt = data.table(dat)
#             
#             setkey(dt, year)
#             
#             facs = names(dt)[!names(dt) %in% c("year", "data")]
#             
#             dt_out = dt[, {
#               .sd = .SD[order(year)]
#               y = .sd$year
#               z = .sd$data
#               k = length(y)
#               
#               if (k < n) {
#                 return(data.table())
#               }
#               
#               idx_end = n:k
#               slopes = vapply(idx_end, function(i) {
#                 ii = (i - n + 1):i
#                 coef(lm(z[ii] ~ y[ii]))[[2]]
#               }, numeric(1))
#               
#               data.table(year = y[idx_end], data = slopes)
#             }, by = facs]
#             
#             rtnFlq = as.FLQuant(data.frame(dt_out))
#             units(rtnFlq) = "slope"
#             
#             rtnFlq})

# setMethod("trend", signature(object = "numeric"),
#           function(object, n = 3) {
#             
#             year = seq_along(object)
#             
#             k = length(year)
#             if (k < n)
#               stop("'n' longer than length(object)")
#             
#             idx_end = n:k
#             slopes = vapply(idx_end, function(i) {
#               ii = (i - n + 1):i
#               coef(lm(object[ii] ~ year[ii]))[[2]]
#             }, numeric(1))
#             
#             slopes})
