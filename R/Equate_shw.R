#' Equate_shw
#'
#' This function extracts two tests' delta estimates and errors from corresponding .shw files in 'output' folder and performs chi-square tests (DIF analysis) on their anchors' delta estimate difference. An Excel file with test results and flags can be saved in 'equating' folder. Also, one plot is saved in subfolder 'plot' inside 'equating' folder.
#'
#' @param folder Folder where two tests' .shw (.del also for step anchors) files are located. Default is 'output' folder.
#' @param test Name of test.
#' @param vars Vector of length 2 such as c('VIC','NSW'). This should exist in .shw file (.del also for step anchors) such as 'test_VIC.shw' and 'test_NSW.shw'. This will also be used in plotting. Order is flexible.
#' @param var_name Character to add before vars in plots. Default is NULL.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 10.
#' @param sav_results TRUE if an Excel file with chi-square test results and a plot are desired. Default is TRUE.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param long_label Whether item labels are longer than 15 characters' fixed width. Default is FALSE.
#' @return Dataframe of chi-square test results for anchors between two tests.
#' @examples
#' Equate_shw(test='elana_math', vars=c('NSW', 'VIC'))
#' Equate_shw(test='elana_math', vars=c('NSW', 'VIC'), long_label=TRUE)
#' Equate_shw(test='elana_math', vars=c('NSW', 'VIC'), steps=TRUE)
#' Equate_shw(test='elana_math', vars=c('NSW', 'VIC'), steps=TRUE, long_label=TRUE)
#' Equate_shw(test='ArabicA', vars=c('3', '4'))
#' @export

Equate_shw <- function(folder=here::here('output'), test, vars, var_name=NULL, p_cut=0.05,
                       chi_cut=10, DIF_cut=0.5, DIF_adj_cut=4,
                       sav_results=TRUE, steps=FALSE, long_label=FALSE){
    if (!dir.exists(here::here('equating'))) dir.create(here::here('equating'))

    r1 <- vars[[1]]
    r2 <- vars[[2]]
    if (!is.null(var_name)) vars <- str_c(var_name, vars)

    # merge data
    if (steps){
        df <- df_del_shw_Step(folder=folder, test=paste0(test, '_', r1),
                              long_label=long_label) %>%
            inner_join(
                df_del_shw_Step(folder=folder, test=paste0(test, '_', r2),
                                long_label=long_label),
                by="iStep"
            )
    } else {
        df <- df_shw(folder=folder, test=paste0(test, '_', r1),
                     long_label=long_label) %>%
            inner_join(
                df_shw(folder=folder, test=paste0(test, '_', r2),
                       long_label=long_label),
                by='item')
    }

    Equate(df=df, test=test, vars=vars, p_cut=p_cut, chi_cut=chi_cut,
           DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut, sav_results=sav_results,
           steps=steps)
}
