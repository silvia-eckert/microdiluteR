#' @title Check monotonicity of well positions across groups
#' @description \code{check_well_positions} checks if well positions across groups, i.e.
#' experiments, monotonically increase or decrease with timepoints measured.
#' @details If non-monotonic groups of well positions are detected, \code{check_well_positions}
#' plots them as line graphs and returns a list with both the corresponding subset of the data for
#' further inspection and the input data adjusted for invalid well positions from visual inspection.
#' @examples
#' # Generate example data
#' set.seed(123)
#' df <- data.frame(Position = rep(1:21, 2),
#'                  Value = c(1:21, sample(1:21,21, TRUE)),
#'                  Timepoint = rep(paste0("T",1:3),14),
#'                  Validity = "valid",
#'                  Group_1 = rep(LETTERS[1:2], each=21),
#'                  Group_2 = rep(letters[1:14], each = 3))
#' # All groups behave monotonically
#' check_well_positions(df[df$Group_1 == "A",],
#'                      x_var = "Timepoint",
#'                      y_var = "Value",
#'                      grouping = c("Group_1", "Group_2"))
#' # Six groups behave non-monotonically
#' check_well_positions(df[df$Group_1 == "B",],
#'                      x_var = "Timepoint",
#'                      y_var = "Value",
#'                      grouping = c("Group_1", "Group_2"))
#' @param input_data A data.frame containing the input data, e.g. from a function call to
#' \code{tidy_single_plate}, \code{tidy_plates_via_params} or \code{tidy_plates_via_prompts}.
#' @param x_var A character string specifying the variable to be plotted on the x-axis. Defaults to
#' 'Timepoint'.
#' @param y_var A character string specifying the variable to be plotted on the y-axis. Defaults to
#' 'Value'.
#' @param v_var A character string specifying the validity information. Usually a column
#' with all rows being 'valid'. Rows are set to 'invalid' based on user selection. Defaults to
#' "Validity".
#' @param wp_var A character string specifying the column providing the well positions. Defaults to
#' "Position".
#' @param grouping A vector of character strings specifying the grouping variables.
#'   Defaults to 'Position' if no grouping is provided.
#' @return \code{check_well_positions} returns a subset of the input data containing
#' only the data from non-monotonic groups, if non-monotonic groups are detected.
#' Otherwise, NULL is returned.
#' @seealso
#' \code{tidy_plate}, \code{tidy_plates_via_params}, \code{tidy_plates_via_prompts}
#' @importFrom ggplot2 geom_line geom_point theme_minimal
#' @importFrom vctrs list_drop_empty
#' @export
# Function to generate line graphs with optional grouping
check_well_positions <- function(input_data,
                                 x_var = "Timepoint",
                                 y_var = "Value",
                                 grouping = "Position",
                                 v_var = "Validity",
                                 wp_var = "Position") {
  
  # Group data if grouping is provided
  if (!is.null(grouping)) {
    data <- split(input_data,
                  do.call(interaction, c(input_data[grouping])))
    data <- list_drop_empty(data)
  } else {
    data <- list(input_data)
    data <- list_drop_empty(data)
  }
  
  # Initialize vector to store indices of non-monotonic groups
  non_monotonic_groups <- c()
  
  # Loop through each group
  for (group_name in names(data)) {
    group <- data[[group_name]]
    
    # Check if values in 'y_var' monotonically increase or decrease across 'x_var'
    is_monotonic <- check_monotonicity(group[[y_var]])
    
    # If values do not monotonically increase or decrease, plot the graph
    if (!is_monotonic) {
      non_monotonic_groups <- c(non_monotonic_groups, group_name)
      
      # Create line chart
      p <- ggplot(group, aes(x = !!sym(x_var),
                             y = !!sym(y_var),
                             group=1)) +
        geom_line() +
        geom_point(size=5) +
        labs(title = paste("Case:", group_name)) +
        theme_minimal()
      
      print(p)
      
      # Ask user if they want to set the group as monotonic
      cat("Non-monotonic well positions detected for case:", group_name, ". Do you want to set this group as valid? (y/n): ")
      choice <- readLines(con = getOption("microdiluteR.connection"), n = 1)
      if (tolower(choice) == "y") {
        # Set the group as monotonic
        cat("Case", group_name, "set as valid\n")
        # Update the data to remove this group from non-monotonic list
        non_monotonic_groups <- non_monotonic_groups[non_monotonic_groups != group_name]
      } else {
        cat("Continuing without changes for case:", group_name, "\n")
      }
    }
  }
  
  # If any group is non-monotonic, return a subset of data containing non-monotonic groups
  if (length(non_monotonic_groups) > 0) {
    cat("Non-monotonic groups detected:", paste(non_monotonic_groups, collapse = ", "), "\n")
    cat("Returning subset of data containing non-monotonic groups.\n")
    
    # Extract rows corresponding to non-monotonic groups
    non_monotonic_subset <- input_data[do.call(paste,
                                             c(input_data[grouping],
                                               sep = ".")) %in% non_monotonic_groups, ]
    # Invalidate samples in non_monotonic_subset and extract well positions
    non_monotonic_subset[[v_var]] <- "invalid"
    invalid_well_positions <- unique(non_monotonic_subset[, wp_var])
    
    # Create unique identifier for each combination of group values
    group_combinations <- do.call(paste, c(input_data[grouping], sep = "."))
    unique_group_combinations <- unique(group_combinations)
    
    # Iterate over each unique group value
    for (i in unique_group_combinations) {
      group_data <- input_data[group_combinations == i, ]
      group_data[[v_var]][group_data[[wp_var]] %in% invalid_well_positions] <- "invalid"
      input_data[group_combinations == i, ] <- group_data
    }

    return(list(non_monotonic_subset = non_monotonic_subset, modified_input_data = input_data))
  } else {
    cat("All groups have monotonic behavior. Returning original input data.\nDo you want to see all plots? (y/n): ")
    choice <- readLines(con = getOption("microdiluteR.connection"), n = 1)
    if (tolower(choice) == "y") {
      for (group_name in names(data)) {
        group <- data[[group_name]]
        
        # Create the plot
        p <- ggplot(group,
                    aes(x = !!sym(x_var),
                        y = !!sym(y_var),
                        group = 1)) +
          geom_line() +
          geom_point(size = 5) +
          labs(title = paste("Case:", group_name)) +
          theme_minimal()
        
        print(p)
        
        # Wait for user input before proceeding to next plot
        cat("Press Enter to continue...")
        invisible(readLines(n = 1))
      }
    }
    # Return full input data in case all groups are monotonic
    return(input_data)
  }
}


#' @title Check if vector values are monotonically increasing or decreasing
#' @description \code{check_monotonicity} checks whether the values in a numeric vector are monotonically
#' increasing or decreasing.
#' @param vec A numeric vector to be checked for monotonicity.
#' @return \code{check_monotonicity} returns a logical value.
#' @details \code{check_monotonicity} checks if all differences between consecutive elements 
#' in the vector 'vec' are non-negative (indicating monotonic non-decreasing 
#' behavior) or non-positive (indicating monotonic non-increasing behavior).
#' @examples
#' # Check if a vector is monotonically increasing (will return TRUE)
#' check_monotonicity(c(1, 2, 3, 4, 5))
#' # Check if a vector is monotonically decreasing (will return FALSE)
#' check_monotonicity(c(5, 80, 3, 2, 1))
#' @seealso
#' \code{\link{validate_cells}}, \code{\link{update_validity}}
#' @rdname check_well_positions
#' @export
check_monotonicity <- function(vec) {
  all(vec == cummin(vec)) || all(vec == cummax(vec))
}