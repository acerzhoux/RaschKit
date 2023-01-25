#' item_fit_opt
#'
#' This function extracts item statistics, fit statistics and option statistics
#' from ConQuest output files in 'output' folder and merges them into one dataframe.
#' This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @return Dataframe of item statistics, fit statistics and option statistics.
#' @examples
#' item_fit_opt(test)
#' @export

item_fit_opt <- function(test){
    item_stats(test) |>
    mutate(
        qOrder = as.character(qOrder)
    ) |>
    left_join(
        fit_stats(test),
        by = 'qOrder'
    ) |>
    left_join(
        opt_stats(test) |>
        dplyr::filter(iScore == 1) |>
        dplyr::select(qOrder, key=resp, pv1Avg),
        by='qOrder'
    )
}
