library(pryr)
library(dplyr)

apply_and_round <- function(func, vector) {
  if (all(vector == floor(vector))) {
    round_digits = 1
  } else {
    round_digits = 2
  }
  return(round(func(vector), digits=round_digits))
}

max_distance <- function(vector) {
  return(max(vector) - min(vector))
}

transform_dataframe_columns <- function(df, summary_df, functions_names, grouping_columns) {
  transfomed_df <- NULL
  for (function_name in functions_names) {
    sliced_columns <- union(
      intersect(paste0(names(df), "_", function_name), names(summary_df)),
      grouping_columns
    )
    sliced_df <- summary_df %>% select(sliced_columns)
    sliced_df["Charactesristics"] <- function_name
    colnames(sliced_df) <- gsub(paste0("(.*)_", function_name), "\\1", colnames(sliced_df))
    transfomed_df <- rbind(transfomed_df, sliced_df)
  }
  col_count <- dim(transfomed_df)[2]  
  transfomed_df <- transfomed_df[, c(col_count, col_count - 1, col_count - 2, 1:(col_count - 3))]
  return(transfomed_df)
}

get_columns_statistics <- function(df, grouping_columns) {
  Min <- partial(apply_and_round, min)
  Max <- partial(apply_and_round, max)
  Range <- partial(apply_and_round, max_distance)
  Mean <- partial(apply_and_round, mean)
  Median <- partial(apply_and_round, median)
  Variance <- partial(apply_and_round, var)
  Standard.Deviation <- partial(apply_and_round, sd)
  functions_names <- c("Min", "Max", "Range", "Mean", "Variance", "Standard.Deviation")
  summary_df <- summarize_if(df, is.numeric, functions_names)
  return(transform_dataframe_columns(df, summary_df, functions_names, grouping_columns))
} 

df <- read.table(
  "http://www.math.uni.wroc.pl/~dyba/materials/R/szalwie.csv",
  sep=";",
  header=TRUE,
  encoding="UTF-8",
  dec=","
)
summary1_df <- get_columns_statistics(df, c())
summary2_df <- get_columns_statistics(df %>% group_by(miejsce), c("miejsce"))
summary3_df <- get_columns_statistics(df %>% group_by(miejsce, kwadrat), c("miejsce", "kwadrat"))

#rm(list = ls())