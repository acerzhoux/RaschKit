#' item_fit_opt
#'
#' This function extracts item statistics, fit statistics and option statistics from ConQuest output files in 'output' folder and merges them into one dataframe. This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 'test'.
#' @param test Name of test.
#' @return Dataframe of item statistics, fit statistics and option statistics.
#' @examples
#' item_fit_opt()

item_fit_opt <- function(folder, test){
    item_stats(folder=folder, test=test) %>%
        left_join(fit_stats(folder=folder, test=test), by='qOrder') %>%
        left_join(opt_stats(folder=folder, test=test) %>%
                      filter(iScore == 1) %>%
                      select(qOrder, key=resp, pv1Avg),
                  by='qOrder')
}
