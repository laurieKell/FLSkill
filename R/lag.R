lag<-function(x, y, lagX=0, lagY=0) {
  # Lag vector x by lagX
  laggedX=if (lagX>0) {
    c(rep(NA, lagX), head(x, -lagX))
  } else if (lagX < 0) {
    c(tail(x, lagX), rep(NA, -lagX))
  } else {
    x
  }
  # Lag vector y by lagY
  laggedY=if (lagY>0) {
    c(rep(NA, lagY), head(y, -lagY))
  } else if (lagY < 0) {
    c(tail(y, lagY), rep(NA, -lagY))
  } else {
    y
  }
  
  df=data.frame(x = laggedX, y = laggedY)
  df=na.omit(df)  # Remove rows with NAs
  return(df)
}
