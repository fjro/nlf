# Unit test two way functions
#define the functions with associatied parameters to scale the amount of noise added
linear <- function(x, noise, noiseLevel, numNoise, n) {x + noise *(noiseLevel/numNoise)* rnorm(n)}

quadratic <- function(x, noise, noiseLevel, numNoise, n) { 4*(x-.5)^2+  noise * (noiseLevel/numNoise) * rnorm(n)}

cubic <- function(x, noise, noiseLevel, numNoise, n) {
  128*(x-1/3)^3 -48*(x-1/3)^3-12*(x-1/3)+10* noise  * (noiseLevel/numNoise) *rnorm(n)
}
qroot <- function(x, noise, noiseLevel, numNoise, n) {
  x^(1/4) + noise * (noiseLevel/numNoise) *rnorm(n)
}

exponential2 <- function(x, noise, noiseLevel, numNoise, n){ exp(x^2) + (1.5 *noise * (noiseLevel/numNoise) * rnorm(n))}
logE <- function(x, noise, noiseLevel, numNoise, n) { log(x) + 2 * (noise * (noiseLevel/numNoise) * rnorm(n))}

sigmoid <- function(x, noise, noiseLevel, numNoise, n) {((1 + exp(10*(0.5 - x)))^-1) +( noise * (noiseLevel/numNoise) * rnorm(n))}
step <- function(x, noise, noiseLevel, numNoise, n) { (x > 0.5) + noise*5*noiseLevel/numNoise *rnorm(n) }

spike <- function(x, noise, noiseLevel, numNoise, n) {
  v <- vector('numeric', n)
  for (i in seq_along(x)) {
    if (x[i] < 0.05) {
      v[i] <- 4
    } else if(x[i] < 0.1) {
      v[i] <- -18*x[i] + 1.9
    } else {
      v[i] <- -x[i]/9 + 1/9
    }
  }
  
  v + noise*5*noiseLevel/numNoise *rnorm(n)
}

sinLow <- function(x, noise, noiseLevel, numNoise, n) { sin(4*pi*x) + 2*noise * (noiseLevel/numNoise) *rnorm(n)}

sinHigh <- function(x, noise, noiseLevel, numNoise, n) { sin(16*pi*x) + 2*noise * (noiseLevel/numNoise) *rnorm(n)}
linearPeriodic <- function(x, noise, noiseLevel, numNoise, n) { sin(10*pi*x) + x + 3*noise * (noiseLevel/numNoise) *rnorm(n)}

varyingFreq <- function(x, noise, noiseLevel, numNoise, n) { sin(5*pi*x*(1+x)) + x + 3*noise * (noiseLevel/numNoise) *rnorm(n)}

circle <- function(x, noise, noiseLevel, numNoise, n) {(2*rbinom(n,1,0.5)-1) * (sqrt(1 - (2*x - 1)^2)) + noise/4*noiseLevel/numNoise *rnorm(n)}
xShaped <- function(x, noise, noiseLevel, numNoise, n) {((4*(x-.5)^2 + (noiseLevel/numNoise) * rnorm(n)) * sample( c(-1,1), size=n, replace=T ) )}


test_that("linear implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- linear(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- linearCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("quadratic implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- quadratic(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- quadraticCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("cubic implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- cubic(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- cubicCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("qroot implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- qroot(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- qrootCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("exponential implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- exponential2(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- exponentialCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp, tolerance = .000001)
})

test_that("Natural log implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- logE(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- logECpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Sigmoid implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sigmoid(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sigmoidCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Step implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- step(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- stepCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Spike implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- spike(x, 2, 10, 30, 10)
  set.seed(1)
  ycpp <- spikeCpp(x, 2, 10, 30, 10)
  expect_equal(y, ycpp, tolerance = 0.01)
})

test_that("Sine low implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sinLow(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sinLowCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Sine high implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- sinHigh(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- sinHighCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Linear periodic implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- linearPeriodic(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- linearPeriodicCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

test_that("Varying freq implementations are equal", {
  set.seed(1)
  x <- runif(10)
  set.seed(1)
  y <- varyingFreq(x, 3, 10, 30, 10)
  set.seed(1)
  ycpp <- varyingFreqCpp(x, 3, 10, 30, 10)
  expect_equal(y, ycpp)
})

#circle and x rely on random sampling so testing is awkward


