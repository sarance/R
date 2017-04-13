#function that takes in vector of data and a coefficient,
#returns boolean vector if a certain point is an outlier or not
check_outlier <- function(v, coef = 1.5) {
  quantiles <- quantile(v, probs = c(0.25, 0.75))
  IQR <- quantiles[2] - quantiles[1]
  res <- v < (quantiles[1] - coef * IQR)|v > (quantiles[2] + coef * IQR)
  return(res)
}