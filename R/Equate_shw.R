#' Equate_shw
#'
#' This function extracts two tests' delta estimates and errors from
#' corresponding .shw files in 'output' folder and performs chi-square tests
#' (DIF analysis) on their anchors' delta estimate difference. An Excel file
#' with test results and flags can be saved in 'equating' folder. Also, one
#' plot is saved in subfolder 'plot' inside 'equating' folder.
#'
#' @param test Name of test.
#' @param vars Vector of length 2 such as c('VIC','NSW'). This should exist in
#' .shw file (.del also for step anchors) such as 'test_VIC.shw' and 'test_NSW.shw'.
#' This will also be used in plotting. Order is flexible.
#' @param var_name Character to add before vars in plots. Default is NULL.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 10.
#' @param sav_results TRUE if an Excel file with chi-square test results and
#' a plot are desired. Default is TRUE.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE.
#' @return Dataframe of chi-square test results for anchors between two tests.
#' @examples
#' Equate_shw(test='elana_math', vars=c('NSW', 'VIC'))
#' @export

Equate_shw <- function(test, vars, var_name=NULL, p_cut=0.05,
                       DIF_cut=0.5, DIF_adj_cut=4,
                       sav_results=TRUE, step=FALSE, iterative=FALSE){
  if (!dir.exists('equating')) dir.create('equating')

  r1 <- vars[[1]]
  r2 <- vars[[2]]
  if (!is.null(var_name)) vars <- str_c(var_name, vars)

  # merge data
  if (step){
    df <- inner_join(
      df_del_shw_Step('output', paste0(test, '_', r1)),
      df_del_shw_Step('output', paste0(test, '_', r2)),
      by="iStep"
    ) |>
    na.omit()
  } else {
    df <- inner_join(
      df_shw('output', paste0(test, '_', r1)),
      df_shw('output', paste0(test, '_', r2)),
      by='item'
    ) |>
    na.omit()
  }

  Equate(df, test, vars, p_cut, DIF_cut, DIF_adj_cut,
         sav_results, 1, step, FALSE, iterative)
}
