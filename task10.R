library(dplyr)
library(cowplot)
library(sp)
library(maptools)


barplot_continents <- function (islands_df) {
  continents_df <- filter(islands_df, continent == "continent")
  par(mfrow = c(1, 1), mar = c(5, 8, 5, 5))
  barplot(
    continents_df$Square,
    main = "Area of continents",
    horiz = TRUE,
    names.arg = continents_df$Islands,
    col = c("blue", "darkorange", "yellow4", "darkgreen", "brown", "purple", "tomato"),
    xlab = expression(paste("Area (in thousands of ", km^2, ")")),
    las=2
  )
}


piechart_continents <- function (islands_df) {
  continents_df <- filter(islands_df, continent == "continent")
  continents_df$Percentage <- round(continents_df$Square / sum(continents_df$Square) * 100, digits = 1)
  colors <- c("deepskyblue1", "orange", "yellow", "green", "maroon1", "purple", "tomato")
  par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
  pie(
    continents_df$Square,
    paste0(continents_df$Percentage, "%"),
    col = colors,
    main = "Area of continents"
  )
  legend(
    "bottomright",
    legend = continents_df$Islands,
    cex = 2.0,
    fill = colors,
    text.width = 0.55
  )
}


barplot_islands <- function(islands_df) {
  canada_islands_df <- filter(islands_df, Country == "Canada") %>% top_n(6, Square) %>% arrange(Square)
  indonesia_islands_df <- filter(islands_df, Country == "Indonesia") %>% top_n(6, Square) %>% arrange(Square)
  islands_matrix <- matrix(c(canada_islands_df$Square, indonesia_islands_df$Square), ncol = 2)
  colors <- c("blue", "darkorange")
  par(mfrow = c(1, 1), mar = c(8, 5, 5, 5))
  barplot(
    t(islands_matrix),
    main = "Comparison of largest Canada and Indonesia islands",
    beside = TRUE,
    names.arg = paste(c("6th", "5th", "4th", "3rd", "2nd", "1st"), "largest"),
    col = colors,
    ylab = expression(paste("Area (in thousands of ", km^2, ")")),
    las=2
  )
  legend(
    "topleft",
    legend = c("Canda islands", "Indonesia islands"),
    cex = 2.0,
    fill = colors,
    text.width = 3.5
  )
}


mosaicplot_islands <- function (islands_df) {
  canada_islands_df <- filter(islands_df, Country == "Canada") %>% top_n(6, Square) %>% arrange(Square)
  indonesia_islands_df <- filter(islands_df, Country == "Indonesia") %>% top_n(6, Square) %>% arrange(Square)
  islands_matrix <- matrix(c(canada_islands_df$Square, indonesia_islands_df$Square), ncol = 2)
  rownames(islands_matrix) <- paste(canada_islands_df$Islands, "/", indonesia_islands_df$Islands)
  colnames(islands_matrix) <- c("Canada", "Indonesia")
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
  mosaicplot(
    t(islands_matrix),
    main = "Comparison of largest Canada and Indonesia islands",
    col = c("purple", "tomato"),
    xlab = "Country",
    ylab = "Canada island / Indonesia island",
    las = 2,
    cex.axis = 0.8,
  )
}


scatterplot_szalwies <- function (szalwies_df) {
  szalwies_matrix <- matrix(
    c(szalwies_df$wys, szalwies_df$lisc.dl, szalwies_df$lisc.szer, szalwies_df$liczba.nerwow),
    ncol = 4
  )
  colnames(szalwies_matrix) <- c("Height", "Length of leaf", "Width of leaf", "Number of nerves")
  par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))
  pairs(szalwies_matrix, main = "Scatterplots of pairs of salvias features", col = c("orange", "blue"), pch = 8)
  par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), xpd = FALSE)
  plot(szalwies_df$lisc.dl ~ szalwies_df$lisc.szer,  col = c("orange", "blue"), pch = 8,
       main = "Leaves lengths and widths of salvies", xlab = "Length of leaf", ylab = "Width of leaf")
  abline(lm(szalwies_df$lisc.dl ~ szalwies_df$lisc.szer), lwd = 2, col = "red")
}


histogram_szalwies <- function (szalwies_df) {
  par(mfrow = c(2, 2), mar=c(5, 5, 5, 5))
  hist(szalwies_df$wys, freq = FALSE, main = "Height of salvias", col="darkorange", xlab = "Height",
       ylab = "Probability")
  height_distribution <- function(x) dnorm(x, mean = mean(szalwies_df$wys), sd = sd(szalwies_df$wys))
  curve(height_distribution, col="darkred", lwd = 3, add = TRUE)
  hist(szalwies_df$lisc.dl, freq = FALSE, main = "Leaves lengths of salvias", col = "darkorange", ylim = c(0, 0.09),
       xlab = "Leaf length", ylab = "Probability")
  leaf_length_distribution <- function(x) dnorm(x, mean = mean(szalwies_df$lisc.dl), sd = sd(szalwies_df$lisc.dl))
  curve(leaf_length_distribution, col="darkred", lwd = 3, add = TRUE)
  hist(szalwies_df$lisc.szer, freq = FALSE, main = "Leaves widths of salvias", col = "darkorange",
       xlab = "Leaf width", ylab = "Probability")
  leaf_width_distribution <- function(x) dnorm(x, mean = mean(szalwies_df$lisc.szer), sd = sd(szalwies_df$lisc.szer))
  curve(leaf_width_distribution, col="darkred", lwd = 3, add = TRUE)
  hist(szalwies_df$liczba.nerwow, freq = FALSE, main = "Number of nerves of salvias", col = "darkorange",
       xlab = "Number of nerves", ylab = "Probability")
  leaf_nerves_distribution <- function(x) dnorm(
    x, mean = mean(szalwies_df$liczba.nerwow), sd = sd(szalwies_df$liczba.nerwow)
  )
  curve(leaf_nerves_distribution, col="darkred", lwd = 3, add = TRUE)
}


show_maps <- function () {
  data(wrld_simpl)
  par(mfrow= c(1, 1), mar=c(4, 4, 4, 4))
  plot(wrld_simpl, xlim = c(4, 54), ylim = c(-33, 33), bg='white', col='gray', border='black',
       main = "Map of Africa by 2008 nominal GDP (billions USD)")
  plot(wrld_simpl[c(180, 153),], col = "black", add = TRUE)
  plot(wrld_simpl[c(2, 50),], col = "darkgreen", add = TRUE)
  plot(wrld_simpl[c(6, 190, 107, 190, 114),], col = "darkolivegreen", add = TRUE)
  plot(wrld_simpl[c(56, 92, 205, 87, 35),], col = "darkolivegreen4", add = TRUE)
  plot(wrld_simpl[c(183, 69, 28, 27, 67, 52, 206, 223, 182),], col = "darkolivegreen3", add = TRUE)
  plot(wrld_simpl[c(108, 122, 218, 210),], col = "darkolivegreen2", add = TRUE)
  plot(wrld_simpl[c(104, 168, 113, 126, 36, 19, 76, 185, 199),], col = "darkseagreen1", add = TRUE)
  plot(wrld_simpl[c(54, 40, 181, 224, 221, 116),], col = "honeydew", add = TRUE)
  legend(
    "bottomleft",
    cex = 2.0,
    text.width = 10.0,
    legend = c("200+", "100-200", "50-100", "20-50", "10-20", "5-10", "1-5", "-1"),
    fill = c("black", "darkgreen", "darkolivegreen", "darkolivegreen4", "darkolivegreen3", "darkolivegreen2",
             "darkolivegreen1", "honeydew"),
  )
}


islands_df <- read.table("lab9_images/Islands.csv", sep=";", header=TRUE, encoding="UTF-8")
szalwies_df <- read.table(
  "http://www.math.uni.wroc.pl/~dyba/materials/R/szalwie.csv",
  sep=";",
  dec = ",",
  header=TRUE,
  encoding="UTF-8"
)
barplot_continents(islands_df)
piechart_continents(islands_df)
barplot_islands(islands_df)
mosaicplot_islands(islands_df)
scatterplot_szalwies(szalwies_df)
histogram_szalwies(szalwies_df)
show_maps()
rm(list = ls())