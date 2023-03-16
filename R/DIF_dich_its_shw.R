#' DIF_dich_its_shw
#'
#' This function performs chi-square tests (DIF analysis) on all items's
#' difference of delta estimates between two groups of test takers.
#' An Excel file with test results and flags is saved in 'DIF' folder.
#' Also, scatterplot of delta  before and after review is saved in
#' subfolder 'plot' inside 'DIF' folder.
#'
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars Vector of DIF categories, e.g., c('Girls','Boys'). Order should
#' correspond to alphabetic/numeric order of DIF variables' two categories' code in data.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param facil_cut Threshold of number of percent to flag an item with large
#' facility difference between two groups of test takers. Default is 10.
#' @param save_xlsx Whether to save summary file and plots. Default is TRUE
#' (one DIF variable).
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return List of summary of results from dichotomous DIF variable analysis,
#' including comments, step, summary statistics with flags, and statistics of
#' items after review.
#' @examples
#' # Not run
#' # DIF_dich_its_shw(DIFVar='Gender', test='AHU', vars=c('Male', 'Female'))
#' @export

DIF_dich_its_shw <- function(DIFVar, test, vars,
               p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4,
               desig_effect=1, step=FALSE, facil_cut=10,
               save_xlsx=TRUE, iterative=FALSE, quick=TRUE){
  # DIF: delta
  if (step){
    # use item*step*DIFVar estimates
    df <- delta_DIF_dich_step(test, DIFVar, quick)
  } else {
    df <- delta_DIF_dich(test, DIFVar, quick) %>%
      bind_cols(
        delta_error_DIF_dich(paste0('DIF/', DIFVar), test)
      ) %>%
      select(item, everything())

    id_na <- which(apply(select(df, -item), 1, function(x) any(is.na(x))))
    if (length(id_na)){
      df[id_na,] %>%
        write_xlsx(paste0('DIF/', DIFVar, '_', test, '_iRemoved', '.xlsx'))
      df <- na.omit(df)
    }
  }

  DIF_dich(DIFVar, test, vars, df, p_cut, DIF_cut, DIF_adj_cut,
       desig_effect, step, facil_cut, iterative, save_xlsx, quick)
}
