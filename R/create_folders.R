#' create_folders
#'
#' This function creates folders for a pineline of psychometric analyses.
#'
#' This function create folders to put different types of files. The folders
#' are named as 'data', 'dataRaw', 'DIF', 'equating', 'input', 'output', 'rCode',
#' and 'results'. 'dataRaw' contains raw dataset files received. 'data' contains
#' processed dataset, key, and label files (.txt) that correspond to a test
#' such as 'HUM_1' (Humanities, Grade 1). 'DIF' and 'equating' contain table
#' and plot results (.xlsx, .pdf) of DIF and equating analyses. 'input' contains
#' ConQuest input files of control files (.cqc), label files (.lab), and perhaps
#' anchor files (.anc). 'output' contains output files from ConQuest. 'rCode'
#' contains R code files (.R). 'results' contains summary files.
#'
#' @param DIFVar Name of DIF variable for DIF analysis. Default is NULL. When a
#' name is provided, a folder with that name will be created under 'DIF' folder
#' to hold DIF analysis output files and analysis results.
#' @export

create_folders <- function(DIFVar=NULL){
    folders <- c('dataRaw', 'input', 'output',
                 'results', 'equating', 'DIF',
                 if(!is.null(DIFVar)) paste0('DIF/', DIFVar))
    for(i in folders) if(!dir.exists(i)) dir.create(i)

    tryCatch({
        fs::dir_copy(system.file("rCode", package = "RaschKit"), getwd())
        fs::dir_copy(system.file("data", package = "RaschKit"), getwd())
    },
    error = function(e){
    })
}
