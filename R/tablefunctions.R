#' GetTable1
#'
#' @description A function that outputs a table of basic descriptive statistics
#'   for specified variables. A numeric variable will return a mean +/- 1 SD. A
#'   character/factor variable will return counts. The goal is to make it easy
#'   to create a descriptive table from a freshly imported dataset with
#'   potentially misleading variable types. The function will automatically
#'   coerce variables to a certain variable type, numeric/character/factor,
#'   given your tolerance.
#' @param data The dataset you want to describe
#' @param vars A character or character vector. This should be the names of the
#'   variables that you want to describe.
#' @param tolerance A numeric value. This is used to help the function determine
#'   which variables are truly numeric or character variables. Your tolerance is
#'   the number of different values that your categorical variables have. For
#'   example, if you have a gender variable of 1 = Male and 2 = Female, then you
#'   have 2 different values. Your tolerance should be based on the categorical
#'   variable with the most different values.
#' @return Returns a dataframe of the descriptive statistics for export.

getTable1 <- function(data, vars, tolerance = 20)
{
  # Initialize the temporary objects
  temp_dataframe <- as.data.frame(data) %>%
    dplyr::select(!!vars)
  temp_num_vars <- NULL
  temp_fac_vars <- NULL

  # Subset the data frame into 2 data frames. One for numeric variables and the other for character variables
  for(i in 1:length(vars))
  {
    if(is.numeric(temp_dataframe[, i]))
    {
      if(length(table(temp_dataframe[, i])) > tolerance)
      {
        temp_num_vars <- c(temp_num_vars, colnames(temp_dataframe)[i])
      } else {
        temp_dataframe[, i] <- as.character(temp_dataframe[, i])
      }
    }


    if(is.character(temp_dataframe[, i]) || is.factor(temp_dataframe[, i]))
    {

      if(length(table(temp_dataframe[, i])) > tolerance)
        # If the number of factors in a variable exceeds 20 (or tolerance), then the variable will be converted to numeric
        # (assuming that >20 factor levels means numeric values stored as characters).
        # Otherwise, it will be assumed to be a true categorical variable
      {
        temp_dataframe[, i] <- as.numeric( as.character(temp_dataframe[, i]))
        temp_num_vars <- c(temp_num_vars, colnames(temp_dataframe[i]))

        temp_variable_name <- colnames(temp_dataframe[i])
        warning <- paste("WARNING: The variable, ", temp_variable_name, ", has > ", tolerance, " factors. Attempting to convert it to a numeric variable.")
        writeLines("")
        print(warning)
        writeLines("")
      }
      else
      {
        temp_fac_vars <- c(temp_fac_vars, colnames(temp_dataframe[i]))
      }
    }
  }

  # NUMERIC VARIABLES

  # Subset the data frame into only numerical variables, transpose the dataframe into long format, omit the missing values,
  # group by the variables to attain the mean and standard deviation.
  if(length(temp_num_vars) > 0)
  {
    numeric_table <- temp_dataframe %>%
      dplyr::select(!!temp_num_vars) %>%
      gather(temp_num_vars[1]:temp_num_vars[length(temp_num_vars)], key = variable, value = value) %>%
      na.omit() %>%
      group_by(variable) %>%
      summarise(mean = mean(value), stand_dev = sd(value))

    # Round the values to 3 digits and format into a clean output
    numeric_table$mean <- round(numeric_table$mean, 3)
    numeric_table$stand_dev <- round(numeric_table$stand_dev, 3)
    numeric_table$factor <- "NONE"
    numeric_table$output <- paste(numeric_table$mean, "+/-", numeric_table$stand_dev)

    numeric_table <- numeric_table %>%
      dplyr::select(variable, factor, output)

    numeric_table <- as.data.frame(numeric_table) # Need to convert to dataframe to later concatenate
  }

  # FACTOR VARIABLES

  # Subset the data frame into only categorical variables, transpose the dataframe into long format, omit the missing values,
  # group by the categorical variable and group again by the factor levels, get the number of observations in each factor level,
  # then get the proportion in each factor level.
  if(length(temp_fac_vars) > 0)
  {
    factor_table <- temp_dataframe %>%
      dplyr::select(!!temp_fac_vars) %>%
      gather(temp_fac_vars[1]:temp_fac_vars[length(temp_fac_vars)], key = variable, value = factor) %>%
      na.omit() %>%
      group_by(variable, factor) %>%
      summarise(count = length(variable)) %>%
      mutate(total = sum(count), proportion = count/total)

    # Round the percentage to 4 digits and format into a clean output
    factor_table$proportion <- round(factor_table$proportion, 4)
    factor_table$proportion <- factor_table$proportion*100
    factor_table$output <- paste(factor_table$count, " (", factor_table$proportion, "%)")

    factor_table <- factor_table %>%
      dplyr::select(variable, factor, output)

    factor_table <- as.data.frame(factor_table) # Need to convert to dataframe to later concatenate
  }

  # CONCATENATE THE TABLES (if they exist)
  if(length(temp_num_vars) > 0 && length(temp_fac_vars) > 0)
  {
    table1 <- rbind(numeric_table, factor_table)
  }
  if(length(temp_num_vars) > 0 && length(temp_fac_vars) == 0)
  {
    table1 <- numeric_table
  }
  if(length(temp_num_vars) == 0 && length(temp_fac_vars) > 0)
  {
    table1 <- factor_table
  }

  ordering <- data.frame(variable = vars, order = 1:length(vars))
  ordering$variable <- as.character(ordering$variable)

  ordered_table1 <- table1 %>%
    left_join(ordering, by = "variable") %>%
    arrange(order) %>%
    dplyr::select(variable, factor, output)

  return(ordered_table1)

}
