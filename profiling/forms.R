#script to plot each form with noise
functions <- c("linear", "quadratic", "cubic", "qroot", "exponential2", "logE", "sigmoid", "step", "spike", "sinLow", "sinHigh",
               "linearPeriodic", "varyingFreq", "circle", "xShaped")

simFun <- function(fn, n = 300, noise = 3, noiseLevel = 1, numNoise = 30) {
  f <- match.fun(fn)
  x <- runif(n)
  res <- data.frame(f(x, noise, noiseLevel, numNoise, n))
  colnames(res) <- 'y'
  res$x <- x
  res$Function <- fn
  res$noise <- noiseLevel
  res$n <- n
  res
}


#run the benchmarks and aggregate the results
results <- sapply(1:3, function(l) lapply(functions, simFun))
results <- plyr::ldply(results)

library(ggplot2)
ggplot(results, aes(x, y, colour=noise)) + 
  geom_point(alpha=0.3) +
  facet_wrap(~Function, scales = "free_y") +
  theme_bw()
