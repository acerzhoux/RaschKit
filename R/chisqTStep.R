#' chisqTStep
#'
#' This function does chi-square test on two groups of delta estimates of
#' item step parameters. This is used for equating between step anchors of two tests.
#'
#' @param df Datarame of step anchors' delta and error estimates in two tests
#' (.x, .y), with variable names of 'delta.x', 'delta.y', 'error.x', and 'error.y'.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @return Dataframe of chi-square test results on step estimates.
#' @examples
#' chisqTStep()
#' @export

chisqTStep <- function(df, desig_effect=1){
  if (nrow(df) != nrow(na.omit(df))) {
    stop('Missing in data! Remove and retry?')
  }

  modify_at(
    df,
    c('delta.x', 'delta.y', 'error.x', 'error.y'),
    as.numeric
  ) |>
  mutate(
    error.x=sqrt(desig_effect)*error.x,
    error.y=sqrt(desig_effect)*error.y,
    error=sqrt(error.x^2 + error.y^2),
    delta.x_dev=delta.x - mean(delta.x),
    delta.y_dev=delta.y - mean(delta.y),
    LI=(delta.x_dev + delta.y_dev)/2 - error,
    UI=(delta.x_dev + delta.y_dev)/2 + error,
    DIF=delta.x_dev - delta.y_dev,
    DIF_std=DIF / error,
    chisq=DIF_std^2,
    p=pchisq(chisq, df=1, lower.tail=FALSE)) |>
    modify_if(is.numeric, ~round(.x, 3)
 )
}
