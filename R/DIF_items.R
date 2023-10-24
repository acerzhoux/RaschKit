#' DIF_items
#'
#' This function finds out items showing DIF between two tests.

#' @param df Dataframe with results of chi-square tests on items that
#' appeared in two tests.
#' @param p_cut p value of chi-square test.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests.
#' @return Dataframe with results of chi-square tests that satisfy all thresholds.

DIF_items <- function(df, p_cut, DIF_cut, DIF_std_cut){
  df %>%
    filter(p < p_cut,
     (DIF_std > DIF_std_cut | DIF_std < -DIF_std_cut),
     (DIF > DIF_cut | DIF < -DIF_cut))
}
