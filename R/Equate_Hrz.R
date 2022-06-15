#' Equate_Hrz
#'
#' This function performs horizontal equating. An Excel file with 'Equating_Horizontal' in name is saved in 'equating' folder, each sheet of which is for one grade. Also, one plot is saved in subfolder 'plot' inside 'equating' folder for each grade.
#'
#' @param folder Folder where all grades' calibration results on both Form A and Form B are located. Default is 'output' folder.
#' @param test Name of test.
#' @param grades Vector of all grades. Default is c(2:10).
#' @param forms Forms used in a test. Default is c('A','B')
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference between two tests. Default is 4.
#' @param steps TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param long_label Whether item labels are longer than 15 characters' fixed width. Default is FALSE.
#' @return List of chi-square test results on Form A and Form B for each grade.
#' @examples
#' Equate_Hrz(test='ArabicA', grades=c(3:6))
#' @export

Equate_Hrz <- function(folder=here::here('output'), test, grades=c(2:10),
                      forms=c('A','B'), p_cut=0.05, chi_cut=10,
                      DIF_cut=0.5, DIF_adj_cut=4, steps=FALSE, long_label=FALSE){
    # grades, forms: combined in file names (Test_2A,Test_2B, ...)
    if (!dir.exists(here::here('equating'))) dir.create(here::here('equating'))

    grps <- list()
    for (i in seq_along(grades)){
        grps[[i]] <- str_c(grades[[i]], forms)
    }

    equat_ls <- list()
    for (i in seq_along(grps)){
        grade_form_2 <- grps[[i]]
        cat('Equating Forms', grade_form_2, '...\n')
        sht <- paste0(grade_form_2, collapse='_')
        equat_ls[[sht]] <- Equate_shw(folder=folder, test=test, vars=grade_form_2,
                                      p_cut=p_cut, DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut,
                                      sav_results=FALSE, steps=steps, long_label=long_label)
    }

    # save results
    writexl::write_xlsx(map(equat_ls, 'results'),
                        here::here('equating', paste0('Equating_Horizontal_', test,
                                                      if(steps) '_Steps', '.xlsx')))
    pdf(file.path(here::here('equating', paste0('Equating_Horizontal_', test,
                                                if(steps) '_Steps', '.pdf'))),
        width=20/2.54, height=20/2.54)
    map(map(equat_ls, 'plot'), print)
    dev.off()

    return(map(equat_ls, 'results'))
}
