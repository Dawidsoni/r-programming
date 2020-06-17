library(ggplot2)
library(dplyr)


a_vector <- sample(seq(0.25, 5, 0.25), size = 10, replace = FALSE)
args <- seq(-3, 3, 0.01)
df <- data.frame(
  args = args,
  values1 = a_vector[1] * args - args ^ 3,
  values2 = exp(-a_vector[5] * abs(args)) * sin(args),
  values3 = sin(args) * sin(a_vector[4] * args),
  values4a = sin(a_vector[9] * args) - args,
  values4b = cos(a_vector[9] * args) - 1,
  values5a = a_vector[10] * cos(args) ^ 3,
  values5b = a_vector[10] * sin(args) ^ 3
)
sample_df <- df %>% sample_n(size = 30)
group1_df <- df %>% select(x = values4a, y = values4b) %>% sample_n(size = 30)
group1_df$group <- "4th function"
group2_df <- df %>% select(x = values5a, y = values5b) %>% sample_n(size = 30)
group2_df$group <- "5th function"
groups_df <- union(group1_df, group2_df)


ggplot(groups_df) +
  geom_point(aes(x = x, y = y, color = factor(group)), shape = 17, size = 2.0, alpha = 0.7) +
  theme(legend.position = "top") +
  labs(color = "Sample points of 4th and 5th function") +
  xlab('X axis') +
  ylab('Y axis')

ggplot(df) +
  geom_line(aes(x = args, y = values2, color = "Second function"), lwd = 1.5, linetype = "dotdash") +
  geom_line(aes(x = args, y = values3, color = "Third function"), lwd = 1.0) +
  ggtitle("Comparison of the second and third function") +
  xlab("X axis") +
  ylab("Y axis") +
  scale_color_manual(name = "Colors", values = c("Second function" = "darkorange", "Third function" = "magenta")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = sample_df) +
  geom_step(aes(x = args, y = values1), color = "darkblue") +
  ggtitle("Step plot of samples of the first function") +
  xlab("X axis") +
  ylab("Y axis") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = sample_df) +
  geom_point(aes(x = args, y = values2), fill = "darkred", color = "black", shape = 22, size = 3) +
  geom_line(aes(x = args, y = values2), color = "darkgreen") +
  ggtitle("Plot of the second function") +
  labs(x = "Argument", y = "Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = sample_df) +
  geom_line(aes(x = args, y = abs(values3)), color = "red", size = 3.5, linetype = "longdash") +
  geom_area(aes(x = args, y = abs(values3)), fill = "darkorange") +
  ggtitle("Plot of area of the absolute value of the third function") +
  labs(x = "Argument", y = "Area") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

islands_df <- read.table("lab9_images/Islands.csv", sep=";", header=TRUE, encoding="UTF-8")
continents_df <- filter(islands_df, continent == "continent") %>% rename(Continents = Islands)

ggplot(continents_df, aes(x = Continents, y = Square, fill = Continents)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Blues") +
  ggtitle("Area of continents") +
  labs(y = expression(paste("Area (in thousands of ", km^2, ")")), x = "") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(continents_df, aes(x = "", y = Square, fill = Continents)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("deepskyblue1", "orange", "yellow", "green", "maroon1", "purple", "tomato")) +
  ggtitle("Area of continents") +
  labs(y = expression(paste("Area (in thousands of ", km^2, ")")), x = "") +
  theme(plot.title = element_text(hjust = 0.5))

szalwies_df <- read.table(
  "http://www.math.uni.wroc.pl/~dyba/materials/R/szalwie.csv",
  sep=";",
  dec = ",",
  header=TRUE,
  encoding="UTF-8"
)

ggplot(szalwies_df, aes(x = wys)) +
  geom_histogram(aes(y = ..density..), bins = 12, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(szalwies_df$wys), sd = sd(szalwies_df$wys)),
                color = "darkorange", lwd = 1.5) +
  ggtitle("Height of salvias") +
  labs(x = "Height", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(szalwies_df, aes(x = lisc.dl)) +
  geom_histogram(aes(y = ..density..), bins = 12, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(szalwies_df$lisc.dl), sd = sd(szalwies_df$lisc.dl)),
              color = "darkorange", lwd = 1.5) +
  ggtitle("Leaves lengths of salvias") +
  labs(x = "Leaf length", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(szalwies_df, aes(x = lisc.szer)) +
  geom_histogram(aes(y = ..density..), bins = 12, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(szalwies_df$lisc.szer), sd = sd(szalwies_df$lisc.szer)),
                color = "darkorange", lwd = 1.5) +
  ggtitle("Leaves widths of salvias") +
  labs(x = "Leaf width", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(szalwies_df, aes(x = liczba.nerwow)) +
  geom_histogram(aes(y = ..density..), bins = 12, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(szalwies_df$liczba.nerwow), sd = sd(szalwies_df$liczba.nerwow)),
                color = "darkorange", lwd = 1.5) +
  ggtitle("Number of nerves of salvias") +
  labs(x = "Number of nerves", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))

rm(list = ls())
