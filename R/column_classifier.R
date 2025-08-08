#' Classify the type of each column in a data frame
#'
#' This function takes a data frame as input and returns a data frame
#' where each row corresponds to a column in the input.
#'
#' @param x A data frame whose columns are to be classified.
#' @importFrom dplyr arrange
#' @return A data.frame where each row contains the column name and its type.
#' @export

column_classifier <- function(x){
  # Check if the input is a data frame
  if (!is.data.frame(x)) {
    stop("Input must be a data frame.")
  }

  # Initialize an empty list to store the types of each column
  column_types <- vector("list", ncol(x))

  # Loop through each column and classify its type
  for (i in seq_along(x)) {
    column_types[[i]] <- class(x[[i]])
  }

  # Convert the list to a data frame
  result <- data.frame(column = names(x), type = unlist(column_types),
                       stringsAsFactors = FALSE) %>%
    arrange(type, column)

  return(result)
}
