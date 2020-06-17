acotan <- Vectorize(function(x) 1 / atan(x))

# Task 1

par(mfrow=c(1, 1))
curve(asin, from = -1, to = 1, ylim = c(-pi, pi), col = "orange", lwd = 3, axes = FALSE)
curve(acos, from = -1, to = 1, col = "blue", add = TRUE, lwd = 3, lty = 4)
curve(atan, from = -3, to = 3, col = "purple", add = TRUE, lty = 6, lwd = 4)
curve(acotan, from = -3, to = 3, col = "green", add = TRUE, lty = 2, lwd = 4)
axis(1, at = c(-1, -0.5, 0, 0.5, 1), labels = expression(-1, -1 / 2, 0, 1 / 2, 1))
axis(2, at = c(-pi, -pi / 2, 0, pi / 2, pi), labels = expression(-pi, -pi / 2, 0, pi / 2, pi))
title(main = "Inverses of trygonometric functions")
legend("bottomright", legend=c("arcsin(x)", "arccos(x)", "arctan(x)", "arcctg(x)"),
       col=c("orange", "blue", "purple", "green"), lty=c(1, 4, 6, 2), lwd=c(3, 3, 3, 3), cex=2.0,
       adj = c(0.1, 0.5), bg = "lightblue")
text(0.92 , -1.9, labels = "Legend", cex = 1.8)


# Task 2

sample_rnorm <- function() {
  return(cumsum(rnorm(100)))
}


trajectories <- matrix(
  c(sample_rnorm(), sample_rnorm(), sample_rnorm(), sample_rnorm(), sample_rnorm()),
  ncol = 5
)
matplot(trajectories, type = "l", axes = FALSE)
title(main = "Brownian motion")
axis(1, at = seq(0, 100, 10))
axis(2, at = seq(floor(min(trajectories)), floor(max(trajectories) + 5), 5))
rounded_min_value <- round(min(trajectories), digits = 2)
text(10 , min(trajectories), labels = paste("Minimum is achieved at", rounded_min_value), cex = 1.2)
mtext("5 simulated trajectories")

# Task 3

plot(c(0, 100), c(0, 100), type = "n")
rect(40, 40, 60, 60, col="purple")
abline(a = 0, b = 1, lty = 1, lwd = 3, col = "red")
abline(a = 100, b = -1, lty = 1, lwd = 3, col = "red")
lines(c(50, 50), c(-20, 120), lty = 2, lwd = 5, col="blue")
lines(c(-20, 120), c(50, 50), lty = 2, lwd = 5, col="blue")
symbols(30, 30, circles = 7, bg = "yellow", add = TRUE, inches = FALSE)
symbols(70, 70, squares = 10, bg = "green", add = TRUE, inches = FALSE)
symbols(70, 30, stars = matrix(7, ncol = 6), bg = "orange", add = TRUE, inches = FALSE)
symbols(30, 70, thermometers = matrix(c(5, 12, 0.1), ncol = 3), bg = "brown", add = TRUE, inches = FALSE)
polygon(c(25, 45, 45, 35, 25), c(5, 5, 20, 12.5, 20), col = "magenta")

# Task 4

elliptic_x <- seq(-2, 2, 0.03)
elliptic_y <- seq(-2, 2, 0.03)
elliptic_f <- function(x, y) (x^2 + y^2)
elliptic_z <- outer(elliptic_x, elliptic_y, elliptic_f)

hyperbolic_x <- seq(-2, 2, 0.03)
hyperbolic_y <- seq(-2, 2, 0.03)
hyperbolic_f <- function(x, y) (x^2 - y^2)
hyperbolic_z <- outer(hyperbolic_x, hyperbolic_y, hyperbolic_f)

par(mfrow=c(1, 2))
persp(elliptic_x, elliptic_y, elliptic_z, theta = 5, phi = 30, expand = 0.8, border = "darkblue",
      xlab = "X axis", ylab = "Y axis", zlab = "Z axis", ticktype = "detailed", nticks = 5)
title("Elliptic paraboloid (for a = 1, b = 1)")
persp(hyperbolic_x, hyperbolic_y, hyperbolic_z, theta = 40, phi = 40, expand = 0.8, border = "darkorange",
      xlab = "X axis", ylab = "Y axis", zlab = "Z axis", ticktype = "detailed", nticks = 5)
title("Hyperbolic paraboloid (for a = 1, b = 1)")

par(mfrow=c(1, 2))
contour(elliptic_x, elliptic_y, elliptic_z, nlevels = 20, drawlabels = FALSE, col = heat.colors(20), lwd = 3)
title("Contours of elliptic paraboloid (for a = 1, b = 1)")
contour(hyperbolic_x, hyperbolic_y, hyperbolic_z, nlevels = 20, drawlabels = FALSE, col = heat.colors(20), lwd = 3)
title("Contours of hyperbolic paraboloid (for a = 1, b = 1)")

par(mfrow=c(1, 2))
image(elliptic_x, elliptic_y, elliptic_z, col = heat.colors(15))
title("Image plot of elliptic paraboloid (for a = 1, b = 1)")
image(hyperbolic_x, hyperbolic_y, hyperbolic_z, col = heat.colors(15))
title("Image plot of hyperbolic paraboloid (for a = 1, b = 1)")

rm(list = ls())
