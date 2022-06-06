stability <- function(Z){
  M <- nrow(Z)
  d <- ncol(Z)
  
  k_bar_d <- sum(Z)/(M*d)

  p_f <- colMeans(Z)
  
  s2_f <- M/(M-1)*p_f*(1 - p_f)
  
  measure <- 1 - mean(s2_f)/(k_bar_d*(1-k_bar_d))
  
  return(measure)
}
