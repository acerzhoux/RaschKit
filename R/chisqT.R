#' chisqT
#'
#' This function does chi-square test on two groups of delta estimates.
#' This is used for equating between anchors of two tests.
#'
#' @param df Datarame of anchors' delta and error estimates in two tests
#' (.x, .y), with variable names of 'delta.x', 'delta.y', 'error.x', and 'error.y'.
#' @param sigma Indicator of how 'delta.y' is scaled. If TRUE, it is scaled
#' to have same mean and sd as 'delta.x'. If FALSE, it has same mean as 'delta.x'.
#' Default is FALSE.
#' @return Dataframe of chi-square test results.
#' @examples
#' chisqT(df, T)
#' @export

chisqT <- function(df, sigma=FALSE){
  df <- df |>
    modify_at(c('delta.x', 'delta.y', 'error.x', 'error.y'), as.numeric)

  if (nrow(df) != nrow(na.omit(df))) {
    stop('Missing in data! Remove and retry?')
  }

  m.x <- mean(df$delta.x)
  m.y <- mean(df$delta.y)
  shift <- m.x - m.y

  if (sigma){
    sd.x <- sd(df$delta.x)
    sd.y <- sd(df$delta.y)

    df <- mutate(
      df,
      delta.y_adj=(delta.y-m.y)/(sd.y/sd.x) + m.x
    )
  } else {
    df <- mutate(
      df,
      delta.y_adj=delta.y + shift
    )
  }

  mutate(
    df,
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
