#' DIF_dich_its_shw
#'
#' This function performs chi-square tests (DIF analysis) on all items's
#' difference of delta estimates between two groups of test takers.
#' An Excel file with test results and flags is saved in 'DIF' folder.
#' Also, scatterplot of delta  before and after review is saved in
#' subfolder 'plot' inside 'DIF' folder.
#'
#' @param folder Where to read item or step estimates. Default is NULL.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars Vector of DIF categories, e.g., c('Girls','Boys'). Order should
#' correspond to alphabetic/numeric order of DIF variables' two categories' code in data.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests.
#' Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param facil_cut Threshold of number of percent to flag an item with large
#' facility difference between two groups of test takers. Default is 10.
#' @param long_label Whether item labels are longer than 11 characters' fixed
#' width. Default is FALSE.
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

DIF_dich_its_shw <- function(folder=NULL, DIFVar, test, vars,
                             p_cut=0.05, chi_cut=10, DIF_cut=0.5, DIF_adj_cut=4,
                             desig_effect=1, step=FALSE, facil_cut=10,
                             long_label=FALSE, save_xlsx=TRUE,
                             iterative=FALSE, quick=TRUE){
    if (is.null(folder)) folder <- here::here('DIF', DIFVar)

    # DIF: delta
    if (step){
        # use item*step*DIFVar estimates
        df <- delta_DIF_dich_step(folder=folder, test=test,
                                  DIFVar=DIFVar, quick=quick)
    } else {
        df <- delta_DIF_dich(folder=folder, test=test,
                             DIFVar=DIFVar, quick=quick) %>%
            bind_cols(delta_error_DIF_dich(folder=folder, test=test,
                                           long_label=long_label)) %>%
            select(item, everything())

        id_na <- which(apply(select(df, -item), 1, function(x) any(is.na(x))))
        if (length(id_na)){
            df[id_na,] %>%
                write_xlsx(here::here('DIF', str_c(DIFVar, '_', test, '_iRemoved', '.xlsx')))
            df <- na.omit(df)
        }
    }

    DIF_dich(DIFVar=DIFVar, test=test, vars=vars, df=df,
             p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut,
             DIF_adj_cut=DIF_adj_cut, desig_effect=desig_effect,
             step=step, facil_cut=facil_cut,
             long_label=long_label, save_xlsx=save_xlsx,
             iterative=iterative, quick=quick)
}
