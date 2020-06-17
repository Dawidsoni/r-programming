bprint <- function(num, value) {
  print(paste0(num, ") ", round(value, digits=2)))
}


plot_curves <- function(func1, func2, func3, func4, from_range, to_range, max_value, title) {
  plot(c(from_range, to_range), c(0, max_value), type="n", xlab="x", ylab="density", main=title)
  curve(func1, from_range, to_range, col="yellow", lwd=2, add=TRUE)
  curve(func2, from_range, to_range, col="red", lwd=2, add=TRUE)
  curve(func3, from_range, to_range, col="green", lwd=2, add=TRUE)
  curve(func4, from_range, to_range, col="blue", lwd=2, add=TRUE)      
}


plot_points <- function(values1, values2, values3, values4, title) {
  max_value = max(c(values1, values2, values3, values4))
  plot(c(0, length(values1)), c(0, max_value), type="n", xlab="x", ylab="density", main=title)
  points(values1, col="red", pch=4)
  points(values2, col="green", pch=4)
  points(values3, col="blue", pch=4)
  points(values4, col="yellow", pch=4)
}


# Part 1
bprint(1, pexp(0.5, rate=2.5))
bprint(2, ppois(8, lambda=0.125))
bprint(3, pchisq(5, df=20, lower.tail=FALSE))
bprint(4, pgeom(4, prob=0.125, lower.tail=FALSE))
bprint(5, qgamma(1/3, shape=4, rate=8))
bprint(6, qhyper(2/3, m=15, n=10, k=12))
bprint(7, qlogis(0.6, location=-2, scale=9, lower.tail=FALSE))
bprint(8, qbinom(0.25, size=100, prob=1/7, lower.tail=FALSE))
bprint(9, dcauchy(18, location=-3, scale=9))
bprint(10, dnbinom(2, size=20, prob=0.75))
bprint(11, pf(1.5, df1=8, df2=16) - pf(1, df1=8, df2=16))
bprint(12, pt(0.5, df=10) - pt(-0.5, df=10))
bprint(13, ppois(10, lambda=2) - ppois(4, lambda=2))
bprint(14, max(dbinom(0:50, size=50, prob=0.375)))

# Part 2
sample1 <- rbeta(100, shape1=5, shape=10)
sample2 <- rgeom(200, prob=0.75)
sample3 <- rbinom(500, size=1, p=0.2)

# Part 3
plot_curves(
  function(x) dnorm(x, mean=0, sd=1), function(x) dnorm(x, mean=0, sd=0.25),
  function(x) dnorm(x, mean=-2, sd=0.5), function(x) dnorm(x, mean=-2, sd=0.25),
  from_range=-3, to_range=3, max_value=2, title="Normal distribution"
)
plot_curves(
  function(x) dlnorm(x, meanlog=0, sdlog=1), function(x) dlnorm(x, meanlog=0, sdlog=0.3),
  function(x) dlnorm(x, meanlog=-2, sdlog=0.9), function(x) dlnorm(x, meanlog=-2, sdlog=0.6),
  from_range=-0, to_range=3, max_value=6, title="Log-normal distribution"
)
plot_curves(
  function(x) dlogis(x, location=0, scale=1), function(x) dlogis(x, location=0, scale=2),
  function(x) dlogis(x, location=2, scale=1), function(x) dlogis(x, location=2, scale=2),
  from_range=-3, to_range=6, max_value=0.3, title="Logistic distribution"
)
plot_curves(
  function(x) dt(x, df=1), function(x) dt(x, df=3),
  function(x) dt(x, df=5), function(x) dt(x, df=10),
  from_range=-4, to_range=4, max_value=0.5, title="Student's distribution"
)
plot_curves(
  function(x) dunif(x, min=-2, max=2), function(x) dunif(x, min=0, max=1),
  function(x) dunif(x, min=-1, max=1), function(x) dunif(x, min=-3, max=3),
  from_range=-4, to_range=4, max_value=1.2, title="Uniform distribution"
)
plot_curves(
  function(x) dchisq(x, df=1), function(x) dchisq(x, df=3),
  function(x) dchisq(x, df=5), function(x) dchisq(x, df=10),
  from_range=0, to_range=7, max_value=1.2, title="Chi-squared distribution"
)
plot_curves(
  function(x) df(x, df1=1, df2=1), function(x) df(x, df1=3, df2=1),
  function(x) df(x, df1=1, df2=3), function(x) df(x, df1=10, df2=10),
  from_range=0, to_range=3, max_value=2.2, title="Fisher distribution"
)
plot_curves(
  function(x) dcauchy(x, location=0, scale=1), function(x) dcauchy(x, location=0, scale=2),
  function(x) dcauchy(x, location=2, scale=1), function(x) dcauchy(x, location=2, scale=2),
  from_range=-4, to_range=6, max_value=0.4, title="Cauchy distribution"
)
plot_curves(
  function(x) dgamma(x, shape=1, rate=1), function(x) dgamma(x, shape=1, rate=2),
  function(x) dgamma(x, shape=2, rate=1), function(x) dgamma(x, shape=2, rate=2),
  from_range=0, to_range=4, max_value=2, title="Gamma distribution"
)
plot_curves(
  function(x) dbeta(x, shape1=1, shape2=1), function(x) dbeta(x, shape1=3, shape2=0.5),
  function(x) dbeta(x, shape1=0.5, shape2=3), function(x) dbeta(x, shape1=3, shape2=3),
  from_range=0, to_range=1, max_value=3, title="Beta distribution"
)
plot_curves(
  function(x) dexp(x, rate=1), function(x) dexp(x, rate=1.5),
  function(x) dexp(x, rate=2), function(x) dexp(x, rate=2.5),
  from_range=0, to_range=5, max_value=2, title="Exponential distribution"
)
plot_curves(
  function(x) dweibull(x, shape=1, scale=1), function(x) dweibull(x, shape=1, scale=5),
  function(x) dweibull(x, shape=3, scale=1), function(x) dweibull(x, shape=3, scale=5),
  from_range=0, to_range=5, max_value=1.5, title="Weibull distribution"
)

# Part 4
plot_points(
  dbinom(0:20, size=10, prob=0.25), dbinom(0:20, size=10, prob=0.5),
  dbinom(0:20, size=20, prob=0.25), dbinom(0:20, size=20, prob=0.5),
  title="Binomal distribution"
)
plot_points(
  dnbinom(0:80, size=10, prob=0.25), dnbinom(0:80, size=10, prob=0.5),
  dnbinom(0:80, size=20, prob=0.25), dnbinom(0:80, size=20, prob=0.5),
  title="Negative binomal distribution"
)
plot_points(
  dgeom(0:20, prob=0.1), dgeom(0:20, prob=0.2),
  dgeom(0:20, prob=0.3), dgeom(0:20, prob=0.4),
  title="Geometric distribution"
)
plot_points(
  dhyper(0:30, n=500, m=50, k=100), dhyper(0:30, n=500, m=100, k=60),
  dhyper(0:30, n=600, m=20, k=50), dhyper(0:30, n=600, m=40, k=80),
  title="Hypergeometric distribution"
)
plot_points(
  dpois(0:20, lambda=1), dpois(0:20, lambda=4),
  dpois(0:20, lambda=10), dpois(0:20, lambda=12),
  title="Poisson distribution"
)

# Cleaning
rm(list=ls())