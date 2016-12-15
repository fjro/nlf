functions <- c("linear", "quadratic", "cubic", "qroot", "exponential2", "logE", "sigmoid", "step", "spike", "sinLow", "sinHigh",
               "linearPeriodic", "varyingFreq", "circle", "xShaped")

simFun <- function(fn, n = 300, noise = 3, noiseLevel = 1, numNoise = 30) {
  f <- match.fun(fn)
  x <- runif(n)
  res <- data.frame(f(x, noise, noiseLevel, numNoise, n))
  colnames(res) <- 'y'
  res$x <- x
  res$Function <- fn
  res$n <- n
  res
}


#run the benchmarks and aggregate the results
results <- lapply(functions, simFun, n=n)
results <- plyr::ldply(results)

ggplot(results, aes(x, y)) + 
  geom_point(alpha=0.5) +
  facet_wrap(~Function, scales = "free_y")
