#' Equate
#'
#' This function performs chi-square tests (DIF analysis) on a group of anchor items' difference of delta estimates in two tests. An Excel file with test results and flags can be saved in 'equating' folder. Also, one plot is saved in subfolder 'plot' inside 'equating' folder.
#'
#' @param df Dataframe of five variables of 'item' (anchor labels), 'delta.x', 'error.x', 'delta.y', and 'error.y'. '.x' and '.y' should correspond to order of test names in 'vars'.
#' @param test Name of test such as 'AGAT'.
#' @param vars Vector of length 2 such as c('VIC','NSW'). Its order corresponds to two tests associated with .x and .y.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param sav_results TRUE if an Excel file with chi-square test results and a plot are desired. Default is TRUE.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @return Dataframe of chi-square test results for anchors between two tests.
#' @examples
#' Equate()
#' @export

Equate <- function(df, test, vars, p_cut=0.05, chi_cut=10,
                   DIF_cut=0.5, DIF_adj_cut=4, sav_results=TRUE,
                   desig_effect=1, steps=FALSE){
    folder <- c(here::here('equating'))
    if (!dir.exists(folder)) dir.create(folder)

    # DIF check
    if (steps){
        results <- chi_square_test_step(df=df, desig_effect=desig_effect)
    } else {
        results <- chi_square_test(df=df)
    }

    p1 <- plot_DIF(df=error_band(results), wh='Before', vars=vars,
                   p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut,
                   DIF_adj_cut=DIF_adj_cut, steps=steps, DIF=FALSE)

    # iteraively remove DIF anchor of max chi-sq
    updated <- results
    iDIF <- DIF_items(df=updated, p_cut=p_cut, chi_cut=chi_cut,
                      DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut)
    while (dim(iDIF)[1] != 0){
        if (steps){
            updated <- chi_square_test_step(df={updated %>%
                    filter(chisq!=max(chisq))},
                    desig_effect=desig_effect)
        } else {
            updated <- chi_square_test(df={updated %>%
                    filter(chisq!=max(chisq))})
        }
        iDIF <- DIF_items(df=updated, p_cut=p_cut, chi_cut=chi_cut,
                          DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut)
    }

    # plot non-DIF anchors
    p2 <- plot_DIF(df=error_band(updated), wh='After', vars=vars, steps=steps, DIF=FALSE)
    p_save <- p1 / p2 +
        plot_annotation(title=paste0('Anchor ', if(steps) 'Step ', 'DIF Check for ',
                                     str_to_title(test)),
                        subtitle=paste0(vars[[1]], ' vs. ', vars[[2]]),
                        tag_levels='I')

    # DIF anchors found
    if(steps) {
        iDIF <- setdiff(results$iStep, updated$iStep)
        results_flag <- results %>%
            mutate(flag=ifelse(iStep %in% iDIF, 1, NA))
    } else {
        iDIF <- setdiff(results$item, updated$item)
        results_flag <- results %>%
            mutate(flag=ifelse(item %in% iDIF, 1, NA))
    }

    # save results and plots
    sht <- paste0('Equating_', test, '_', if(steps) 'Steps_', vars[[2]], '_to_', vars[[1]])

    if (sav_results) {
        ggsave(file.path(folder, paste0(sht, '.pdf')), p_save,
               width=20, height=20, units="cm")
        writexl::write_xlsx(results_flag, file.path(folder, paste0(sht, '.xlsx')))
    }

    list(results=results_flag, plot=p_save)
}
