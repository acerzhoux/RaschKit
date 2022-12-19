#' Equate_Hrz
#'
#' This function performs horizontal equating. An Excel file with 
#' 'Equating_Horizontal' in name is saved in 'equating' folder, each sheet of 
#' which is for one grade. Also, one plot is saved in subfolder 'plot' inside 
#' 'equating' folder for each grade.
#'
#' @param folder Folder where all grades' calibration results on both Form A 
#' and Form B are located. Default is 'output' folder.
#' @param test Name of test.
#' @param grades Vector of all grades. Default is c(2:10).
#' @param forms Forms used in a test. Default is c('A','B')
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. 
#' Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between 
#' two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference 
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters. 
#' Default is FALSE.
#' @param long_label Whether item labels are longer than 15 characters' fixed 
#' width. Default is FALSE.
#' @return List of chi-square test results on Form A and Form B for each grade.
#' @examples
#' Equate_Hrz(test='ArabicA', grades=c(3:6))
#' @export

Equate_Hrz <- function(folder=here::here('output'), test, grades=c(2:10),
                      forms=c('A','B'), p_cut=0.05, chi_cut=10,
                      DIF_cut=0.5, DIF_adj_cut=4, step=FALSE, long_label=FALSE){
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
                                      sav_results=FALSE, step=step, long_label=long_label)
    }

    summary <- map(equat_ls, 'shift') %>%
        imap(~mutate(.x, Form=.y)) %>%
        map2(map(equat_ls, 'flag'),
             ~mutate(.x, Links_bfr=nrow(.y),
                     Links_afr=nrow(filter(.y, is.na(flag))),
                     Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%'))) %>%
        reduce(bind_rows) %>%
        select(Form, everything())
    sum_ls <- list(Summary=summary) %>%
        append(map(equat_ls, 'flag'))

    # save results
    path_xlsx <- here::here('equating', paste0('Horizontal_', test,
                            if(step) '_step', '.xlsx'))
    writexl::write_xlsx(sum_ls, path_xlsx)

    path_pdf <- file.path(here::here('equating', paste0('Horizontal_', test,
                          if(step) '_step', '.pdf')))
    pdf(path_pdf, width=7, height=14)
    map(map(equat_ls, 'plot_DIF'), print)
    dev.off()

    # point users to files of varying purposes
    writeLines(c(
        paste0('Anchor DIF analysis for ', test, ' (before horizontal equating):'),
        paste0('\tSummary:\t', path_xlsx),
        paste0('\tPlots:\t\t', path_pdf)
    ))

    # sum_ls
}
