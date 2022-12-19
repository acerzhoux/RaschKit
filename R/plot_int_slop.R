#' plot_int_slop
#'
#' This function draws a scatter plot for two vectors of numbers. This is 
#' associated with test named 'test'. The plot also shows the formula, 
#' squared error explained, and RMSE.
#'
#' @param test Name of test.
#' @param x Vector of numbers that correspond to x-axis.
#' @param y Vector of numbers that correspond to y-axis.
#' @return Scatter plot with annotation information.
#' @examples
#' plot_int_slop()
#' @export

plot_int_slop <- function(test, x, y){
    # https://stackoverflow.com/questions/13114539/how-to-add-rmse-slope-intercept-r2-to-r-plot
    mydata <- tibble(x, y)
    fit <- lm(y~x, data=mydata)
    eqn <- paste0('y=', round(coef(fit)[1], 2), '+', round(coef(fit)[2], 2), 'x', '\n',
                  'r^2', '=', round(summary(fit)$r.squared, 2), '\n',
                  'RMSE', '=', round(sqrt(mean(resid(fit)^2)), 2))

    txt1 <- tibble(x=Inf, y=-Inf, label=eqn)
    ggplot(mydata, aes(x, y))+
        geom_point() +
        geom_smooth(method='lm', se=FALSE) +
        geom_text(aes(label=label), data=txt1, vjust='bottom', hjust='right') +
        labs(title=test) +
        ggthemes::theme_tufte()
}
