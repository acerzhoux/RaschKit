#' chi_square_test_step
#'
#' This function does chi-square test on two groups of delta estimates of item step parameters. This is used for equating between step anchors of two tests.
#'
#' @param df Datarame of step anchors' delta and error estimates in two tests (.x, .y), with variable names of 'delta.x', 'delta.y', 'error.x', and 'error.y'.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @return Dataframe of chi-square test results on step estimates.
#' @examples
#' chi_square_test_step()
#' @export

chi_square_test_step <- function(df, desig_effect=1){
    df %>% mutate(
        error.x=sqrt(desig_effect)*error.x,
        error.y=sqrt(desig_effect)*error.y,
        error=sqrt(error.x^2 + error.y^2),
        delta.x_dev=delta.x - mean(delta.x),
        delta.y_dev=delta.y - mean(delta.y),
        DIF=delta.x_dev - delta.y_dev,
        DIF_std=DIF / error,
        LI=(delta.x_dev + delta.y_dev)/2 - error,
        UI=(delta.x_dev + delta.y_dev)/2 + error,
        chisq=DIF_std^2,
        p=pchisq(chisq, df=1, lower.tail=FALSE)) %>%
        modify_at(-1, ~round(., digits=3))
}
