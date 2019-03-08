# Reference : http://ethen8181.github.io/Business-Analytics/ab_tests/frequentist_ab_test.html
data_power_prop_test <- function(sample_size_p1, sample_size_p2, p1, p2){
  count_a  <- sample_size_p1 * p1
  count_b  <- sample_size_p2 * p2
  p_pooled <- (count_a  + count_b) / (sample_size_p1 + sample_size_p2)
  Z <- (p2 - p1) / sqrt( p_pooled * (1 - p_pooled) * (1 / sample_size_p2 + 1 / sample_size_p1))
  
  # Z corresponds to the mean of the normal distribution
  mean1 <- 0
  mean2 <- Z
  
  x <- seq(-4, 6, 0.1) # use for generating the x axis of the normal distribution
  data <- data.frame(x = x, y1 = dnorm(x, mean1, 1), y2 = dnorm(x, mean2, 1), stringsAsFactors = F)
  return(data)
}