#' z_test
#'
#' @description Two-Sample z-test for Comparing Two Means
#'
#' @param x,y  vectors of observations
#'
#' @param sd an assumed common standard deviation
#'
#' @param delta the mean difference corresponding to the null hypothesis
#'
#' @return List with z-statistics and p-value
#'

z_test <- function(x, y, sd=1, delta=0){

  n_x <- length(x)
  n_y <- length(y)

  # calculate the z-statistic
  z_stat <- (mean(x) - mean(y) - delta) /
    sqrt(sd^2/n_x + sd^2/n_y)


  pvalue <- 2*(1-pnorm(abs(z_stat)))
  #decision <- (pvalue < 0.05)

  return(list(z_stat=z_stat, p.value=pvalue))
}
