#' DIF_dich
#'
#' This function performs DIF analyses on a dichotomous variable and summarizes results.
#'
#' @param folder Folder where subgroups' calibration results are located. Default is the subfolder with name of argument 'DIFVar' within 'DIF' folder (folder=NULL).
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars_dich Vector of categories of DIFVar, e.g., c('girl', 'boy'). This is used in plotting. The order should be the same as '1' and '2' in interaction term of ConQuest's .shw files. Note that ConQuest follows alphabetical rule and uses '1' and '2' in output files for 'F' and 'M' (coded in data for female and male).
#' @param p_cut P value for performing chi-square significance tests. One criterion to flag DIF items. Default is 0.05.
#' @param DIF_cut Threshold of difference between two categories' delta estimates. One criterion to flag DIF items. Default is 0.5.
#' @param DIF_adj_cut Threshold of adjusted difference between two categories' delta estimates. One criterion to flag DIF items. Default is 5.
#' @param chi_cut Chi-square value from chi-square test. One criterion to flag DIF items. Default is 10.
#' @param facil_cut Threshold of number of percent to flag an item with large facility difference between two groups of test takers. Default is 10.
#' @param steps TRUE if the chi-square test is on step parameters of polytomous items. Default is FALSE.
#' @param sav_xlsx TRUE if an Excel file of DIF analysis results are to be saved. Default is FALSE.
#' @return Dataframe of summary of results from dichotomous DIF variable analysis.
#' @examples
#' DIF_dich()

DIF_dich <- function(folder=NULL, DIFVar, test, vars_dich,
                     p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=5, chi_cut=10, facil_cut=10,
                     steps=FALSE, sav_xlsx=TRUE){
    if (is.null(folder)) folder <- here::here('DIF', DIFVar)

    # DIF: delta
    df <- delta_DIF_dich(folder=folder, test=test, DIFVar=DIFVar) %>%
        bind_cols(error_delta_DIF_dich(folder=folder, test=test)) %>%
        select(item, everything())
    results <- chi_square_test(df=df)
    iDIF <- DIF_items(df=results, p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut) %>%
        pull(item)

    # print comments
    paste(c(paste(paste('A total of', length(iDIF), 'items showed DIF')),
            paste0('Please consider removing the DIF items of ', paste(iDIF, collapse=', '), '.')), collapse='. ')

    results_flag <- results %>% mutate(Flagged=ifelse(item %in% iDIF, 'Y', ''))
    p1 <- plot_DIF(df=error_band(results), txt=TRUE, wh='Before', vars=vars_dich,
                   p_cut=p_cut, chi_cut=chi_cut,
                   DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut, steps=steps, DIF=TRUE)

    # DIF: facility
    facilities <- facil_DIF(folder=folder, test=test, DIFVar=DIFVar)
    p2 <- plot_facil(facil_dif=facilities, vars_dich=vars_dich, facil_cut=facil_cut)

    p_save <- p1 / p2 +
        plot_annotation(title=paste0(test, ': Difficulty vs. Facility'),
                        tag_levels='I')

    # save results and plots
    sht <- paste0(DIFVar, '_', if(steps) 'Steps_', test)
    ggsave(here::here('DIF', paste0(sht, '.pdf')), p_save,
           width=20, height=20, units="cm")
    print(p_save)

    if (sav_xlsx) {
        writexl::write_xlsx(results_flag, here::here('DIF', paste0(sht, '.xlsx')))
    }
    results_flag
}
