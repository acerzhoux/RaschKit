#' create_folders
#'
#' This function creates folders for a pineline of psychometric analyses.
#'
#' This function create folders to put different types of files. The folders are named as 'data', 'dataRaw', 'DIF', 'equating', 'input', 'output', 'rCode', and 'results'. 'dataRaw' contains raw dataset files received. 'data' contains processed dataset, key, and label files (.txt) that correspond to a test such as 'HUM_1' (Humanities, Grade 1). 'DIF' and 'equating' contain table and plot results (.xlsx, .pdf) of DIF and equating analyses. 'input' contains ConQuest input files of control files (.cqc), label files (.lab), and perhaps anchor files (.anc). 'output' contains output files from ConQuest. 'rCode' contains R code files (.R). 'results' contains summary files.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @return
#' @examples
#' create_folders()
#' @export

create_folders <- function(wd=here::here(), DIFVar=NULL){
    folders <- c(file.path(wd, 'rCode'),
                 file.path(wd, 'data'),
                 file.path(wd, 'dataRaw'),
                 file.path(wd, 'input'),
                 file.path(wd, 'output'),
                 file.path(wd, 'results'),
                 file.path(wd, 'equating'),
                 file.path(wd, 'DIF'),
                 if(!is.null(DIFVar)) file.path(wd, 'DIF', DIFVar))
    for(i in folders) if(!dir.exists(i)) dir.create(i)
}
