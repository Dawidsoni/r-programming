a <- sample(seq(0.25, 5, 0.25), size = 10, replace = FALSE)

args1 <- seq(-3, 3, 0.01)
values1 <- a[1] * args1 - args1 ^ 3

args2 <- seq(-3, 3, 0.01)
values2 <- (a[2] * args2 ^ 2 - args2 ^ 4)^(1/3)

args3 <- seq(-3 + a[3], 3 + a[3], 0.01)
values3 <- exp(-abs(args3 - a[3]))

args4 <- seq(-3 * pi, 3 * pi, 0.01)
values4 <- sin(args4) * sin(a[4] * args4)

args5 <- seq(-3, 3, 0.01)
values5 <- exp(-a[5] * abs(args5)) * sin(args5)

args6 <- seq(-a[6], a[6], 0.01)
values6 <- acos((a[6] ^ 2 - args6 ^ 2) / (a[6] ^ 2 + args6 ^ 2))

args7 <- seq(-3 * pi, 3 * pi, 0.01)
values7a <- args7 * cos(a[7] * args7)
values7b <- args7 * sin(a[7] * args7)

args8 <- seq(0, 2 * pi, 0.01)
values8a <- a[8] * cos(2 * args8)
values8b <- a[8] * cos(3 * args8)

args9 <- seq(0, 6 * pi, 0.01)
values9a <- sin(a[9] * args9) - args9
values9b <- cos(a[9] * args9) - 1

args10 <- seq(0, 2 * pi, 0.01)
values10a <- a[10] * cos(args10) ^ 3
values10b <- a[10] * sin(args10) ^ 3

par(mfrow=c(1, 3))
plot(args1, values1, asp = 1.0)
plot(args2, values2, type = "l")
plot(args3, values3, col = "green")

par(mfcol=c(2, 1))
plot(args4, values4, pch = 8)
plot(args5, values5, cex = 2, col = "blue")

par(mfrow=c(1,1))
par(mar=c(1.5, 1.5, 1.5, 1.5))
horizontal_screens <- split.screen(c(3, 1))
vertical_screens1 <- split.screen(c(1, 2), screen = horizontal_screens[2])
vertical_screens2 <- split.screen(c(1, 2), screen = horizontal_screens[3])

screen(horizontal_screens[1], new = FALSE)
plot(args6, values6, main = "Function 6", sub = "Description", xlab = "x", ylab = "y")
screen(vertical_screens1[1], new = FALSE)
plot(values7a, values7b, type = "p")
screen(vertical_screens1[2], new = FALSE)
plot(values8a, values8b, type = "l", lty = 3, lwd = 5)
screen(vertical_screens2[1], new = FALSE)
plot(values9a, values9b, type = "o", lty = 2, lwd = 2)
screen(vertical_screens2[2], new = FALSE)
plot(values10a, values10b, type = "s", col = "purple")
close.screen(all.screens = T)

rm(list = ls())