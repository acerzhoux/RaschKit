#' cqc_plot_File
#'
#' This function creates 'test_CCC_plots.cqc' file in 'input' folder. 
#' This output file needs to be imported into and run in the GUI version of ConQuest.

#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of test.
#' @param runs Vector of test forms, e.g., c('2A', '2B', '3A', '3B').
#' @examples
#' cqc_plot_File()
#' @export

cqc_plot_File <- function(wd=here::here(), test, runs){
    # create folders if non-existent
    folder <- file.path(wd, 'Output', 'plot')
    if(!dir.exists(folder)) dir.create(folder)

    path <- file.path(wd, 'Output')
    CQ_FILE <- c(
        paste0('Dofor form=',  paste(runs, collapse = ','), ';'),
        paste0('let test=', test, ';'),
        paste0('let path=', path, ';'),
        'get << %path%\\%test%_%form%_compressed.cqs;',
        'Set warnings=no;',
        'plot mcc! gins=all,bins=6,minscale=-3,maxscale=3,estimates=wle,filesave=yes >> %path%\\plot\\%test%_%form%_;',
        'reset;',
        'enddo;',
        'quit;')

    cqc_path <-  file.path(wd, 'Input', paste0(test, '_CCC_plots.cqc'))
    writeLines(CQ_FILE, cqc_path)
}
