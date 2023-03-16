#' DIF_items
#'
#' This function finds out items showing DIF between two tests.

#' @param df Dataframe with results of chi-square tests on items that
#' appeared in two tests.
#' @param p_cut p value of chi-square test.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests.
#' @return Dataframe with results of chi-square tests that satisfy all thresholds.

DIF_items <- function(df, p_cut, DIF_cut, DIF_adj_cut){
  df %>%
    filter(p < p_cut,
     (DIF_std > DIF_adj_cut | DIF_std < -DIF_adj_cut),
     (DIF > DIF_cut | DIF < -DIF_cut))
}
