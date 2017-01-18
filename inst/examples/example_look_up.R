tempdf <- expand.grid(arg1 = c("A", "B", "C"), arg2 = 1:4, arg3 = 1:5)
tempdf$value <- 1:60

look_up(
  data = tempdf,
  value = "value",
  arg1 = c("A", "B", "C", "B", "A"),
  arg2 = c(1, 1, 3.2, 3.0, 5), 
  arg3 = c(-1, 1, 1, 2, 3)
)

# binning doesn't catch values smaller than the smallest
# reference value
look_up(
  data = tempdf,
  value = "value",
  arg1 = c("A", "B", "C", "B", "A"),
  arg2 = c(1, 1, 3.2, 3.0, 5), 
  arg3 = c(-1, 1, 1, 2, 3),
  bin = TRUE
)
# bin can also be given as a character vector
# to avoid binning all numeric variables
look_up(
  data = tempdf,
  value = "value",
  arg1 = c("A", "B", "C", "B", "A"),
  arg2 = c(1, 1, 3.2, 3.0, 5), 
  arg3 = c(-1, 1, 1, 2, 3),
  bin = c("arg2")
)

age_related_df <- data.frame(age = 10 * 0:9, decade = 1:10)

look_up(age_related_df, age = c(0, 10, 20), value = "decade")

# binning lets us look up values not mentioned in the data frame
look_up(age_related_df, age = c(5, 15, 23.5), 
        value = "decade")
look_up(age_related_df, age = c(5, 15, 23.5), 
        value = "decade", bin = TRUE)
