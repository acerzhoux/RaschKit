#' DIF_items
#'
#' This function finds out items showing DIF between two tests.

#' @param df Dataframe with results of chi-square tests on items that appeared in two tests.
#' @param p_cut p value of chi-square test.
#' @param chi_cut Threshold of chi-square difference between two tests.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests.
#' @return Dataframe with results of chi-square tests that satisfy all thresholds.
#' @examples
#' DIF_items()

DIF_items <- function(df, p_cut, chi_cut, DIF_cut, DIF_adj_cut){
    df %>% filter(p < p_cut, chisq > chi_cut,
       (DIF_std > DIF_adj_cut | DIF_std < -DIF_adj_cut),
       (DIF > DIF_cut | DIF < -DIF_cut))
}
