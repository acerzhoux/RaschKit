#' cqc_plot_File
#'
#' This function creates 'test_CCC_plots.cqc' file in 'input' folder.
#' This output file needs to be imported into and run in the GUI version of ConQuest.
#'
#' @param folder Folder where ConQuest output files are located.
#' @param test Name of test.
#' @param runs Vector of test forms, e.g., c('2A', '2B', '3A', '3B').
#' @examples
#' cqc_plot_File()
#' @export

cqc_plot_File <- function(folder='output', test, runs){
    # create folders if non-existent
    path_plot <- paste0(folder, '/plot')
    if(!dir.exists(path_plot)) dir.create(path_plot)

    CQ_FILE <- c(
        paste0('Dofor form=',  paste(runs, collapse = ','), ';'),
        paste0('let test=', test, ';'),
        paste0('let path=', folder, ';'),
        'get << %path%\\%test%_%form%_compressed.cqs;',
        'Set warnings=no;',
        'plot mcc! gins=all,bins=6,minscale=-3,maxscale=3,estimates=wle,filesave=yes >> %path%\\plot\\%test%_%form%_;',
        'reset;',
        'enddo;',
        'quit;')

    cqc_path <-  paste0('input/', test, '_CCC_plots.cqc')
    writeLines(CQ_FILE, cqc_path)
}
