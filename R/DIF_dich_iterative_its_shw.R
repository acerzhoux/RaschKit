#' DIF_dich_iterative_its_shw
#'
#' This function performs chi-square tests (DIF analysis) on all items's difference of delta estimates between two groups of test takers. An Excel file with test results and flags is saved in 'DIF' folder. Also, scatterplot of delta  before and after review is saved in subfolder 'plot' inside 'DIF' folder.
#'
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars Vector of DIF categories, e.g., c('Girls','Boys'). Order should correspond to alphabetic/numeric order of DIF variables' two categories' code in data.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param facil_cut Threshold of number of percent to flag an item with large facility difference between two groups of test takers. Default is 10.
#' @param plot_facil TRUE if a facilitiy plot is desired of delta estimates for the two DIF variable categories.
#' @param long_label Whether item labels are longer than 11 characters' fixed width. Default is FALSE.
#' @return List of summary of results from dichotomous DIF variable analysis, including comments, steps, summary statistics with flags, and statistics of items after review.
#' @examples
#' DIF_dich_iterative_its_shw(DIFVar='gender', test='N_1', vars=c('Girls', 'Boys'))
#' @export

DIF_dich_iterative_its_shw <- function(DIFVar, test, vars,
                   p_cut=0.05, chi_cut=10, DIF_cut=0.5, DIF_adj_cut=4,
                   desig_effect=1, steps=FALSE, facil_cut=10,
                   plot_facil=TRUE, long_label=FALSE){
    folder <- here::here('DIF', DIFVar)
    # DIF: delta
    if (steps){
        # specify item*step*DIFVar?
    } else {
        df <- delta_DIF_dich(folder=folder, test=test, DIFVar=DIFVar) %>%
            bind_cols(delta_error_DIF_dich(folder=folder, test=test,
                                           long_label=long_label)) %>%
            select(item, everything())
    }

    DIF_dich_iterative(DIFVar=DIFVar, test=test, vars=vars, df=df,
                       p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut,
                       DIF_adj_cut=DIF_adj_cut, desig_effect=desig_effect,
                       steps=steps, facil_cut=facil_cut, plot_facil=plot_facil,
                       long_label=long_label)
}
