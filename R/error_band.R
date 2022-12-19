#' error_band
#'
#' This function produces smoothed upper and lower confidence interval (CI) 
#' values of the average of an item's delta estimates in two tests. Those 
#' values will be used in plotting.

#' @param df Dataframe with results of chi-square tests on items that 
#' appeared in two tests.
#' @return Dataframe of chi-square test results with smoothed upper and lower 
#' confidence interval values added.
#' @export

error_band <- function(df){
    df$LB <- predict(loess(LI ~ delta.x, df), newdata=df$delta.x)
    df$UB <- predict(loess(UI ~ delta.x, df), newdata=df$delta.x)

    df
}
