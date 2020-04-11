get_statistics <- function(name, values, round_places=2) {
  s_min <- min(values)
  s_max <- max(values)
  s_range <- max(values) - min(values)
  s_mean <- mean(values)
  s_median <-median(values)
  s_var <- var(values)
  s_sd <- sd(values)
  stats <- c("min", "max", "range", "mean", "median", "varance", "standard deviation")
  values <- round(c(s_min, s_max, s_range, s_mean, s_median, s_var, s_sd), round_places)
  df <- data.frame(stats, values)
  colnames(df) <- c("Characteristics", name)
  return(df)
}


get_quantitive_columns_info <- function() {
  columns_info <- c()
  columns_info <- rbind(columns_info, c("wys", 2))
  columns_info <- rbind(columns_info, c("lisc.ilosc", 1))
  columns_info <- rbind(columns_info, c("lisc.dl", 2))
  columns_info <- rbind(columns_info, c("lisc.szer", 2))
  columns_info <- rbind(columns_info, c("liczba.nerwow", 1))
  columns_info <- rbind(columns_info, c("kwiatostan.dl", 2))
  columns_info <- rbind(columns_info, c("kwiatostan.liczba.odgalezien", 1))
  columns_info <- rbind(columns_info, c("liczba.rzedow.okolkowych", 1))
  columns_info <- rbind(columns_info, c("liczba.kwiatow", 1))
  columns_info <- rbind(columns_info, c("korona.dl", 2))
  columns_info <- rbind(columns_info, c("kielich.dl", 2))
  columns_info <- rbind(columns_info, c("liczba.kwiatostanow", 1))
  colnames(columns_info) <- c("column_name", "round_places")
  return(columns_info)
}


merge_if_non_empty <- function(df, merging_df, property) {
  if (dim(df)[1] == 0) {
    return(merging_df)
  }
  return(merge(df, merging_df, by=property))
}


get_columns_statistics <- function(df) {
  columns_info <- get_quantitive_columns_info()
  statistics <- data.frame()
  for (i in 1:dim(columns_info)[1]) {
    column_name <- columns_info[i, "column_name"]
    round_places <- as.numeric(columns_info[i, "round_places"])
    local_statistics <- get_statistics(column_name, as.numeric(df[, column_name]), round_places)
    statistics <- merge_if_non_empty(statistics, local_statistics, property="Characteristics")
  }
  return(statistics)
}


get_location_columns_statistics <- function(df) {
  places <- levels(df$miejsce)
  statistics <- data.frame()  
  for (place in places) {
    sliced_df <- df[df$miejsce == place, ]
    local_statistics <- get_columns_statistics(sliced_df)
    local_statistics['miejsce'] <- place
    statistics <- rbind(statistics, local_statistics)
  }
  columns_count <- dim(statistics)[2]
  statistics <- statistics[, c(columns_count, 1:(columns_count - 1))]
  return(statistics)
}


get_location_square_columns_statistics <- function(df) {
  places <- levels(df$miejsce)
  squares <- levels(df$kwadrat)
  statistics <- data.frame()
  for (place in places) {
    for (square in squares) {
      sliced_df <- df[df$miejsce == place & df$kwadrat == square, ]
      if (dim(sliced_df)[1] == 0) {
        next()
      }
      local_statistics <- get_columns_statistics(sliced_df)
      local_statistics['miejsce'] <- place
      local_statistics['kwadrat'] <- square
      mean_statistics <- local_statistics[local_statistics$Characteristics == "mean", ]
      statistics <- rbind(statistics, mean_statistics)
    }
  }
  return(statistics)
}


df <- read.table(
  "http://www.math.uni.wroc.pl/~dyba/materials/R/szalwie.csv",
  sep=";",
  header=TRUE,
  encoding="UTF-8"
)
print(get_columns_statistics(df))
print(get_location_columns_statistics(df))
print(get_location_square_columns_statistics(df))
rm(get_statistics, get_quantitive_columns_info, merge_if_non_empty, get_columns_statistics)
rm(get_location_columns_statistics, get_location_square_columns_statistics, df)