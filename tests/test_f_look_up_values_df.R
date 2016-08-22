
context("test that the data frame look-up function works properly")

## set test sets
tempdf <- expand.grid(arg1 = c("A", "B", "C"), arg2 = 1:4, arg3 = 1:5)
tempdf$value <- 1:60

     
just_numeric <- 1:10
names(just_numeric) <- 10 * 0:9

oned_array <- array(1:10, dim = 10, dimnames = list(age = 10* 0:9))
oned_df <- data.frame(age = 10 * 0:9, value = 1:10)
oned_df_othername <- oned_df
names(oned_df_othername)[2] <- "decade"

multiple_outputs_df <- oned_df_othername
multiple_outputs_df$rev_decade <- 11 - multiple_outputs_df$decade

set.seed(4)
tempdf_rand <- tempdf[sample(1:60), ]
oned_df_rand <- oned_df[sample(1:10), ]

## suppress printing of warnings for this test
oldw <- getOption("warn")
options(warn = -1)

test_that(
	"warnings on out-of-range or non-matched values", {
			
		expect_equal(f_look_up_values_df(
						tempdf, arg1 = c("A"),
						arg2 = c(3), arg3 = c(0.5),
						numeric_cols  = c("arg2", "arg3")),
				as.numeric(NA))
		
		expect_warning(f_look_up_values_df(
						tempdf, arg1 = c("A"),
						arg2 = c(3), arg3 = c(0.5),
						numeric_cols  = c("arg2", "arg3")),
				"no non-missing arguments to max")
		
		expect_equal(f_look_up_values_df(
						tempdf, arg1 = c("Z"),
						arg2 = c(3), arg3 = c(2),
						numeric_cols  = c("arg2", "arg3")),
				as.numeric(NA)) 
		
		expect_warning(f_look_up_values_df(
						tempdf, arg1 = c("Z"),
						arg2 = c(3), arg3 = c(2),
						numeric_cols  = c("arg2", "arg3")),
				"Unmatched value for non-numeric attribute arg1: Z") 
	}
)

test_that(
	"errors on improper input", {
		
		expect_error(f_look_up_values_df(
						tempdf, arg1 = c("A", "B", "C", "B", "A"),
						arg2 = c(1, 1, 3.2, 3.0, 5), 
						arg3 = c(1, 1, 1, 1, 1, 1),
						numeric_cols  = c("arg2", "arg3")),
				"all index collections for attributes must have the same length")
		
		expect_error(f_look_up_values_df(just_numeric, age = c(0, 30, 40)),
				"val_source must be a non-empty data frame")
			
		expect_error(f_look_up_values_df(oned_array, age = c(0, 30, 40)),
				"val_source must be a non-empty data frame")
		## unspecified output_col with multiple possibilities
		expect_error(f_look_up_values_df(multiple_outputs_df, age = c(0, 30, 40)))
		expect_error(f_look_up_values_df(multiple_outputs_df, age = c(0, 30, 40), 
		                                 output_col = "random"),
		             "output column random is not a column name of the data frame")
		expect_error(f_look_up_values_df(oned_df, age = c(0, 30, 40), 
		                                 output_col = "random"),
		             "output column random is not a column name of the data frame")
		
		
	}
)

test_that(
	"correct values with correct input", {

		expect_equal(f_look_up_values_df(
						tempdf, arg1 = c("A", "B", "C", "B", "A"),
						arg2 = c(1, 1, 3.2, 3.0, 5), 
						arg3 = c(1, 1, 1, 2, 3),
						numeric_cols  = c("arg2", "arg3")),
				c(1, 2, 9, 20, 34))
		
		expect_equal(f_look_up_values_df(
						oned_df, age = c(0, 10, 20)),
				c(1,2,3))
		
		expect_equal(f_look_up_values_df(
						oned_df_othername, age = c(0.5, 23, 42.7),
						numeric_cols = "age", output_col = "decade"),
				c(1, 3, 5))

		expect_equal(f_look_up_values_df(
		  oned_df_othername, age = c(0, 10, 20), output_col = "decade"),
		  c(1,2,3))
		
		expect_equal(f_look_up_values_df(
		  oned_df, age = c(0.5, 23, 42.7),
		  numeric_cols = "age"),
		  c(1, 3, 5))

		expect_equal(f_look_up_values_df(
		  multiple_outputs_df, age = c(0, 10, 20), output_col = "rev_decade"),
		  c(10,9,8))
		
		expect_equal(f_look_up_values_df(
		  multiple_outputs_df, age = c(0.5, 23, 42.7),
		  numeric_cols = "age", output_col = "decade"),
		  c(1, 3, 5))
		
				
		expect_equal(f_look_up_values_df(
		     tempdf_rand, arg1 = c("A", "B", "C", "B", "A"),
		     arg2 = c(1, 1, 3.2, 3.0, 5), 
		     arg3 = c(1, 1, 1, 2, 3),
		     numeric_cols  = c("arg2", "arg3")),
		     c(1, 2, 9, 20, 34))
		
		expect_equal(f_look_up_values_df(
		     oned_df_rand, age = c(0, 10, 20)),
		     c(1,2,3))
		
		expect_equal(f_look_up_values_df(
		     oned_df_rand, age = c(0.5, 23, 42.7),
		     numeric_cols = "age"),
		     c(1, 3, 5))
		
			}
)

options(warn = oldw)