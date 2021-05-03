#' run_trial
#'
#' @description  simulation function that calculates the power and type 1 error of a particular set 
#' of assumptions (sample sizes, means, standard deviations).
#'
#' @param n sample sizes in both groups (assumed to be equal)
#' 
#' @param mean_x,mean_y means of both groups
#' 
#' @param sd_x,sd_y standard deviations of both groups  
#' 
#' @param test type of test: "t.test", "wilcox.test" or "z.test"   
#'
#' @param alpha significance level (default is 0.05)  
#'
#' @param n_sim number of simulation repetitions.  
#'
#' @param sim_alpha logical: whether the type 1 error must be simulated (default is TRUE)  
#'
#' @return List with power and type 1 error
#' 
#' @export


run_trial <- function(n, mean_x, mean_y, sd_x, sd_y, test, alpha=0.05, n_sim, sim_alpha=TRUE){
  
  decision <- rep(NA, n_sim)
  for (i in 1:n_sim) {
    x <- rnorm(n, mean = mean_x, sd = sd_x)
    y <- rnorm(n, mean = mean_y, sd = sd_y)
    
    if(test=="t.test"){
      test_res <- t.test(x, y)
    }
    
    if(test=="wilcox.test"){
      test_res <- wilcox.test(x, y)
    }
    
    if(test=="z.test"){
      test_res <- z_test(x, y, sd=sd_x)
    }
    
    decision[i] <- (test_res$p.value < alpha)
  }
  
  power <- mean(decision)
  
  if(sim_alpha){
    decision <- rep(NA, n_sim)
    for (i in 1:n_sim) {
      x <- rnorm(n, mean = mean_x, sd = sd_x)
      y <- rnorm(n, mean = mean_x, sd = sd_x)
      
      if(test=="t.test"){
        test_res <- t.test(x, y)
      }
      
      if(test=="wilcox.test"){
        test_res <- wilcox.test(x, y)
      }
      
      if(test=="z.test"){
        test_res <- z_test(x, y, sd=sd_x)
      }
      
      decision[i] <- (test_res$p.value < alpha)
    }
    
    alpha <- mean(decision)
  } else {
    alpha = "not simulated"
  }
  return(list(power=power, alpha=alpha))
}