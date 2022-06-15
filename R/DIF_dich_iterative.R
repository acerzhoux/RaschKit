#' DIF_dich_iterative
#'
#' This function performs chi-square tests (DIF analysis) on all items's difference of delta estimates between two groups of test takers. An Excel file with test results and flags is saved in 'DIF' folder. Also, one plot is saved in subfolder 'plot' inside 'DIF' folder.
#'
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param df Dataframe with delta estimates and errors of items for both categories of dichomomous variable in a test.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param facil_cut Threshold of number of percent to flag an item with large facility difference between two groups of test takers. Default is 10.
#' @param plot_facil TRUE if a facility plot is desired of delta estimates for the two DIF variable categories.
#' @param long_label Whether item labels are longer than 11 characters' fixed width. Default is FALSE.
#' @return List of summary of results from dichotomous DIF variable analysis, including comments, steps, summary statistics with flags, and statistics of items after review.
#' @examples
#' DIF_dich_iterative(DIFVar='gender', test='N_1', vars=c('Girls', 'Boys'))
#' @export

DIF_dich_iterative <- function(DIFVar, test, vars, df,
                   p_cut=0.05, chi_cut=10, DIF_cut=0.5, DIF_adj_cut=4,
                   desig_effect=1, steps=FALSE, facil_cut=10,
                   plot_facil=TRUE, long_label=FALSE){
    folder <- c(here::here('DIF'))
    if (!dir.exists(folder)) dir.create(folder)

    # DIF: delta
    results <- chi_square_test(df=df)
    iDIF <- DIF_items(df=results, p_cut=p_cut, chi_cut=chi_cut,
                      DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut) %>%
        pull(item)

    # DIF check
    if (steps){
        results <- chi_square_test_step(df=df, desig_effect=desig_effect)
    } else {
        results <- chi_square_test(df=df)
    }

    p1 <- plot_DIF(df=error_band(results), wh='Before', vars=vars,
                   p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut,
                   DIF_adj_cut=DIF_adj_cut, steps=steps, DIF=TRUE)

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
    p2 <- plot_DIF(df=error_band(updated), wh='After', vars=vars, steps=steps, DIF=TRUE)
    p_save <- p1 / p2 +
        plot_annotation(title=paste0(test, ': Item ',
                          if(steps) 'step ', 'DIF Check on ', str_to_title(DIFVar)),
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
    sht <- paste0(DIFVar, if(steps) '_steps', '_', test)
    sht_facil <- paste0(DIFVar, if(steps) '_steps', '_', test, '_Facility')

    ggsave(file.path(folder, paste0(sht, '.pdf')), p_save,
           width=20, height=20, units="cm")
    results <- list(comments = DIF_comment_dich(DIFVar=DIFVar, iDIF=iDIF),
                    steps = DIF_steps_dich(),
                    flags=results_flag,
                    stats_aftr_rvw=updated)
    writexl::write_xlsx(results, here::here('DIF', paste0(sht, '.xlsx')))

    # DIF: facility
    if(plot_facil){
        p_DIF_facil <- plot_facil(test=test, vars=vars, facil_cut=facil_cut,
                                  facil_dif=facil_DIF(folder=file.path(folder, DIFVar),
                                                      test=test, DIFVar=DIFVar,
                                                      long_label=long_label),
                                  steps=steps, DIFVar=DIFVar)
        ggsave(file.path(folder, paste0(sht_facil, '.pdf')), p_DIF_facil,
               width=20, height=20, units="cm")
    }

    results
}
