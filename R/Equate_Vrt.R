#' Equate_Vrt
#'
#' This function performs vertical equating. An Excel file with 'Equating_Vertical' in name is saved in 'equating' folder, each sheet of which is for two adjacent grades. Also, one plot is saved in subfolder 'plot' inside 'equating' folder for two adjacent grades.
#'
#' @param folder Folder where all grades' calibration results are located. Default is 'output' folder.
#' @param test Name of test.
#' @param grades Vector of all grades. Default is c(2:10).
#' @param grade_name Character to add before grade in plots. Default is 'L' (for 'level').
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param long_label Whether item labels are longer than 15 characters' fixed width. Default is FALSE.
#' @return List of chi-square test results for anchors between two adjacent grades.
#' @examples
#' Equate_Vrt(test='ArabicA', grades=c(3:10))
#' @export

Equate_Vrt <- function(folder=here::here('output'), test, grades=c(2:10),
                       grade_name='L', p_cut=0.05, chi_cut=10,
                       DIF_cut=0.5, DIF_adj_cut=4, steps=FALSE, long_label=FALSE){
    if (!dir.exists(here::here('equating'))) dir.create(here::here('equating'))

    n_test <- length(grades)
    equat_ls <- list()
    for (i in 1:(n_test-1)){
        test_2 <- c(grades[[i]], grades[[i+1]])
        cat('Equating Grades',test_2,'...\n')
        sht <- paste0(test_2, collapse='_')
        equat_ls[[sht]] <- Equate_shw(folder=folder, test=test, vars=test_2,
                                      var_name=grade_name, p_cut=p_cut,
                                      DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut,
                                      sav_results=FALSE, steps=steps,
                                      long_label=long_label)
    }

    # save results
    writexl::write_xlsx(map(equat_ls, 'results'),
                        here::here('equating', paste0('Equating_Vertical_', test,
                        if(steps) '_Steps', '.xlsx')))
    pdf(file.path(here::here('equating', paste0('Equating_Vertical_', test,
                                                if(steps) '_Steps', '.pdf'))),
        width=20/2.54, height=20/2.54)
    map(map(equat_ls, 'plot'), print)
    dev.off()

    return(map(equat_ls, 'results'))
}
