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
#' @param chi_cut Threshold of chi-square difference between two tests.
#' Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param DIF TRUE if DIF analysis is performed on a dichotomous DIF variable.
#' Default is FALSE (anchor check).
#' @param cor Correlation of two categories' delta estimates.
#' @param shift Shift of two categories' average delta estimates.
#' @param sdr SD ratio of two categories' delta estimates.
#' @param ax_min Minimum value of x and y axis.
#' @param ax_max Maximum value of x and y axis.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return Plot of DIF analysis.
#' @export

plot_DIF <- function(df, wh, vars, p_cut=0.05, chi_cut=10,
                     DIF_cut=0.5, DIF_adj_cut=4, step=FALSE,
                     DIF=FALSE, cor, shift, sdr,
                     ax_min, ax_max, quick=TRUE) {

    if (step){
        df <- df %>%
            dplyr::select(-delta.x, -delta.y) %>%
            dplyr::rename(delta.x=delta.x_dev,
                          delta.y_adj=delta.y_dev)
    }
    if ('iStep' %in% names(df)){
        df <- df %>%
            dplyr::rename(item=iStep)
    }

    txt1 <- tibble(delta.x=-Inf, delta.y_adj=Inf,
                   label=paste0('Easier for ',vars[1]))
    txt2 <- tibble(delta.x=Inf, delta.y_adj=-Inf,
                   label=paste0('Easier for ',vars[2]))

    p <- ggplot(df, aes(x=delta.x, y=delta.y_adj)) +
        geom_point() +
        lims(x= c(ax_min, ax_max), y = c(ax_min, ax_max)) +
        geom_abline(intercept=0, slope=1, colour='gray') +
        geom_ribbon(aes(ymin=LB, ymax=UB), alpha=0.2) +
        geom_text(aes(label=label), data=txt1, vjust='top', hjust='left') +
        geom_text(aes(label=label), data=txt2, vjust='bottom', hjust='right') +
        labs(title=paste0(wh, ' Review: ', dim(df)[1],
                          if (step) ' Steps' else if (DIF) ' Items' else ' Anchors',
                          ' (cor. = ', cor, ', ',
                          'mean shift = ', shift, ', ',
                          'SD ratio = ', sdr, ')'),
             x=paste0('Item Difficulty (Logits) for ', vars[1],
                      if (step | quick) ' (delta-centred)'),
             y=paste0('Item Difficulty (Logits) for ', vars[2],
                      ' (delta-centred)')) +
        ggthemes::theme_tufte()

    if (wh=='Before'){
        DIF_txt <- DIF_items(df=df, p_cut=p_cut, chi_cut=chi_cut,
                             DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut) %>%
            dplyr::select(item, delta.x, delta.y_adj)
        p <- p +
            ggrepel::geom_label_repel(data=DIF_txt, aes(label=item),
                                      size=2.5, segment.size=0.25, alpha=0.5,
                                      max.overlaps=100) +
            labs(caption=paste0('DIF: abs(delta_dif)>', DIF_cut,
                                ', abs(delta_dif_adj)>', DIF_adj_cut,
                                ', abs(chisq)>', chi_cut,
                                ', p<', p_cut))
    }
    p
}
