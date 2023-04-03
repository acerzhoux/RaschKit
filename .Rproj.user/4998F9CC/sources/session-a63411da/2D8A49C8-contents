#' chisqT
#'
#' This function does chi-square test on two groups of delta estimates.
#' This is used for equating between anchors of two tests.
#'
#' @param df Datarame of anchors' delta and error estimates in two tests
#' (.x, .y), with variable names of 'delta.x', 'delta.y', 'error.x', and 'error.y'.
#' @return Dataframe of chi-square test results.
#' @examples
#' chisqT()
#' @export

chisqT <- function(df){
  df <- df |>
    modify_at(c('delta.x', 'delta.y', 'error.x', 'error.y'), as.numeric)

  shift <- mean(df$delta.x) - mean(df$delta.y)

  mutate(
    df,
    delta.y_adj=delta.y + shift,
    delta_ave=(delta.x + delta.y_adj)/2,
    error=sqrt(error.x^2 + error.y^2),
    LI=delta_ave - error,
    UI=delta_ave + error,
    DIF=delta.x - delta.y_adj,
    DIF_std=DIF/error,
    chisq=DIF_std^2,
    p=pchisq(chisq, df=1, lower.tail=FALSE)
  ) |>
  select(-delta_ave, -error) |>
  select(item, everything()) |>
  modify_if(is.numeric, ~round(.x, 3))
}
