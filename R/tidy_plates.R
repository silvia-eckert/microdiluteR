#' @title Add metadata to values from photometer measurements
#' @description Cleans a list of data frames with different structures. This function reads data
#' from either a folder containing text files or from a list of data frames. It then cleans each
#' data frame using the function \code{tidy_plates_via_prompts()}.
#' @param input_data Either a folder path containing text files or a list of data frames.
#' @param how_many A character vector specifying if metadata should be added to only a single plate or multiple plates.
#' @param user_prompt Logical indicating whether adding metadata should be applied via user prompts. Only applied
#' if \code{user_prompt} is set to \code{TRUE}. Defaults to \code{FALSE}.
#' @param multiple_structures Logical indicating whether adding metadata should be applied for each plate separately,
#' because plates have different metadata structures. Will be applied  via user prompts for each plate separately. Defaults to \code{FALSE}.
#' @param direction A character vector specifying the orientation of the plate layout.
#' It can be either "horizontal" or "vertical".
#' @param ... Additional arguments to be passed to \code{\link{read_plates}}, \code{\link{tidy_single_plate}},
#' \code{\link{tidy_plates_via_params}}, or \code{\link{tidy_plates_via_prompts}}.
#' @return A list of cleaned data frames.
#' @seealso
#' \code{\link{read_plates}}, \code{\link{tidy_plates_via_prompts}}, \code{\link{tidy_plates_via_params}}, \code{\link{tidy_single_plate}}
#' @export
tidy_plates <- function(input_data,
                        how_many = c("single", "multiple"),
                        user_prompt = FALSE,
                        multiple_structures = FALSE,
                        direction = c("horizontal", "vertical"),
                        ...) {

  # Decide based on user input which function to use  
  if (how_many == "single") {
    # Apply tidy_single_plate() to tidy single plate
    tidy_data <- tidy_single_plate(input_data, direction = direction, ...)
  } else if (how_many == "multiple" && user_prompt == FALSE && multiple_structures == FALSE) {
    # Apply tidy_single_plate() to tidy multiple plates with the same metadata structure without user prompts
    tidy_data <- tidy_plates_via_params(input_data, direction = direction, ...)
  } else if (how_many == "multiple" && user_prompt == TRUE && multiple_structures == FALSE) {
    # Apply tidy_single_plate() to tidy multiple plates with the same metadata structure via user prompts
    tidy_data <- tidy_plates_via_prompts(input_data, direction = direction, ...)
  } else if (how_many == "multiple" && user_prompt == TRUE && multiple_structures == TRUE) {
    # If data frames have different structures, apply tidy_plates_via_prompts() to each plate separately.
    raw_data_list <- read_plates(input_data, ...)
    # Loop over and capture metadata separately for each plate
    tidy_list <- list()
    for (i in names(raw_data_list)) {
      cat("Adding metadata for file:", i, "\n")
      tidy_list_element <- tidy_plates_via_prompts(raw_data_list[i], direction = direction, ...)
      tidy_list[[i]] <- tidy_list_element
      }
    tidy_data <- do.call(rbind, tidy_list)
    raw_data_list
    } else {
      stop("Please select the parameters with which you would like to add metadata to the values of your photometer plate(s).")
  }
  return(tidy_data)
}
