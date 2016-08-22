 tempdf <- expand.grid(arg1 = c("A", "B", "C"), arg2 = 1:4, arg3 = 1:5)
 tempdf$value <- 1:60
 
 f_look_up_values_df(tempdf, 
                     arg1 = c("A", "B", "C", "B", "A"),
                     arg2 = c(1, 1, 3.2, 3.0, 5), 
                     arg3 = c(1, 1, 1, 2, 3),
                     numeric_cols  = c("arg2", "arg3"))
 
 age_related_df <- data.frame(age = 10 * 0:9, decade = 1:10)
 
 f_look_up_values_df(age_related_df, age = c(0, 10, 20), output_col = "decade")
 f_look_up_values_df(age_related_df, age = c(5, 15, 23.5), 
                     numeric_cols = "age", output_col = "decade")

## error if you don't specify that a numeric column is numeric
##   and you don't have values that are in the table
\dontrun{
 f_look_up_values_df(age_related_df, age = c(5, 15, 23.5), output_col = "decade")
 }
