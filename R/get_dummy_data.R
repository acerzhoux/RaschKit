#' get_dummy_data
#'
#' This function generate dummy data so that there is one case for one possible
#' total raw score in the test.
#'
#' @param num_items Number of items in the test.
#' @examples
#' a <- get_dummy_data(num_items=60)
#' @export

get_dummy_data <- function(num_items) { # Gemini 3

  # 1. Define Parameters
  num_scores <- num_items + 1

  # 2. Generate and Validate
  # We wrap the generation in a repeat loop to ensure column constraints are met
  repeat {
    data_list <- lapply(0:num_items, function(s) {
      res <- c(rep(1, s), rep(0, num_items - s))
      sample(res)
    })

    responses_matrix <- do.call(rbind, data_list)

    # Calculate Column Sums (Correct answers per item)
    item_totals <- colSums(responses_matrix)

    # VALIDATION:
    # An item is "all correct" if sum == 61
    # An item is "all incorrect" if sum == 0
    if (all(item_totals > 0) && all(item_totals < num_scores)) {
      break # Success! Exit the loop.
    }
  }

  # 3. Finalize Data Frame
  student_id <- sprintf("S%03d", 0:num_items)
  item_names <- sprintf("Item%02d", 1:num_items)

  dummy_data <- data.frame(student_id, responses_matrix)
  colnames(dummy_data) <- c("student_id", item_names)

  # 4. Verification
  col_ranges <- range(colSums(dummy_data[, -1]))
  cat("Item Score Range (Min - Max):", col_ranges, "\n")

  if(col_ranges[1]==0 || col_ranges[2]==num_scores) {
    stop("Failure: items are all-correct or all-incorrect.")
  }

  return(dummy_data)
}
