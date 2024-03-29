#' plot_DIF
#'
#' This function plots all items in a test or all anchors and label DIF items/anchors.
#'
#' @param df Dataframe with results of chi-square tests on items that appeared
#' in two tests.
#' @param wh 'Before' or 'After' when to plot before or after equating or in
#' DIF analysis.
#' @param vars Vector of length 2 such as c('VIC','NSW'). Its order corresponds
#' to .x and .y (delta, error).
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'. Default is NULL.
#' @param cor Correlation of two categories' delta estimates.
#' @param shift Shift of two categories' average delta estimates.
#' @param sdr SD ratio of two categories' delta estimates.
#' @param axRange Vector of range of x and y axis.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return Plot of DIF analysis.
#' @export

plot_DIF <- function(df, wh, vars, p_cut=0.05, DIF_cut=0.5, DIF_std_cut=4,
                     step=FALSE, DIFVar=NULL, cor, shift, sdr, axRange, quick=TRUE) {
  DIF <- ifelse(!is.null(DIFVar), TRUE, FALSE)
  if (step){
    df <- df |>
      dplyr::select(-delta.x, -delta.y) |>
      dplyr::rename(delta.x=delta.x_dev,
              delta.y_adj=delta.y_dev)
  }

  # fit line
  fit <- lm(delta.y_adj~delta.x, data=df)

  # annotation: harder, easier
  txt1 <- tibble(
    delta.x=-Inf,
    delta.y_adj=Inf,
    label=paste0(
      'Easier for ', vars[1], '\n',
      'y=', round(coef(fit)[1], 2), '+', round(coef(fit)[2], 2), 'x', ', ',
      'r^2 = ', round(summary(fit)$r.squared, 2), ', ',
      'RMSE =', round(sqrt(mean(resid(fit)^2)), 2)
    )
  )
  txt2 <- tibble(
    delta.x=Inf,
    delta.y_adj=-Inf,
    label=paste0('Easier for ', vars[2])
  )

  # error line: Both sides
  error <- mean(sqrt(df$error.x^2 + df$error.y^2))

  p <- ggplot(df, aes(x=delta.x, y=delta.y_adj)) +
    geom_point() +
    lims(x=axRange, y=axRange) +
    geom_abline(intercept=0, slope=1, colour='gray') +
    geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], colour='blue') +
    geom_abline(intercept=-1.96*error, slope=1, colour='gray', linetype="dotted") +
    geom_abline(intercept=1.96*error, slope=1, colour='gray', linetype="dotted") +
    geom_ribbon(aes(ymin=LB, ymax=UB), alpha=0.2) +
    geom_text(aes(label=label), data=txt1, vjust='top', hjust='left') +
    geom_text(aes(label=label), data=txt2, vjust='bottom', hjust='right') +
    labs(
      title=paste0(
        wh, ' Review: ', dim(df)[1], if (step) ' Steps' else if (DIF) ' Items' else ' Anchors',
        ' (cor. = ', cor, ', ', 'mean shift = ', shift, ', ', 'SD ratio = ', sdr, ')'
      ),
      x=paste0('Item Difficulty (Logits) for ', vars[1], if (step | DIF) ' (delta-centred)'),
      y=paste0('Item Difficulty (Logits) for ', vars[2], if (step | DIF) ' (delta-centred)' else ' (+ Mean shift)')
    ) +
    ggthemes::theme_tufte()

  if (wh=='Before'){
    DIF_txt <- dplyr::filter(df, flag==1) |>
      dplyr::select(item, delta.x, delta.y_adj)

    p <- p +
      ggrepel::geom_label_repel(
        data=DIF_txt,
        aes(label=item),
        size=2.5,
        segment.size=0.25,
        alpha=0.5,
        max.overlaps=100
      ) +
      labs(
        caption=paste0(
          'DIF: abs(delta dif.) > ', DIF_cut,
          ', abs(delta dif. std.) > ', DIF_std_cut,
          ', p < ', p_cut
        )
      )
  }
  p
}
