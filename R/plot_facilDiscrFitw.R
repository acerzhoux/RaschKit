#' plot_facilDiscrFitw
#'
#' This function plots the scatterplots of facilities, discrimination, and
#' weighted fit of two tests.
#'
#' @param facilDiscrFitw Dataframe of two tests' statistics from its.txt files.
#' @param tests Vector of test names in output files to be used to locate files.
#' @param colsInd1 Two tests' column numbers of 1st index in dataframe.
#' @param numInd Number of index.
#' @export

plot_facilDiscrFitw <- function(facilDiscrFitw, tests, colsInd1, numInd){
  drawPlot <- function(index, cols){
    x <- names(facilDiscrFitw)[cols[[1]]]
    y <- names(facilDiscrFitw)[cols[[2]]]
    indRange <- range(c(facilDiscrFitw[x], facilDiscrFitw[y]))
    ggplot(facilDiscrFitw, aes(!!sym(x), !!sym(y))) +
      geom_point() +
      lims(x=indRange, y=indRange) +
      geom_abline(intercept=0, slope=1, colour='gray') +
      labs(
        title=paste0('Mean shift: ',
                     round(colMeans(facilDiscrFitw[cols[[1]]]-facilDiscrFitw[cols[[2]]]), 3)),
        x=paste0(index, ' for ', tests[1]),
        y=paste0(index, ' for ', tests[2])
      ) +
      ggthemes::theme_tufte()
  }

  if (numInd==1){
    p <- drawPlot('Facility', colsInd1)
  } else if (numInd==2){
    plist <- map2(c('Facility', 'Discrimination'),
                  c(0, 1), ~drawPlot(.x, colsInd1 + .y))
    p <- plist[[1]] / plist[[2]]
  } else if (numInd==3) {
    plist <- map2(c('Facility', 'Discrimination', 'Infit'),
                  c(0, 1, 2), ~drawPlot(.x, colsInd1 + .y))
    p <- plist[[1]] /plist[[2]] / plist[[3]]
  }

  p + plot_annotation(
      titl=paste0('Index comparison: ', tests[1], ' vs. ', tests[2]),
      tag_levels='I'
    )
}
