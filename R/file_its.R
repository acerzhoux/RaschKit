#' file_its
#'
#' This function extracts item analysis statistics from CQ output file 'test.its'. This is associated with test named 'test'.

#' @param folder Folder where subgroups' calibration results are located. Default is the subfolder with name of argument 'DIFVar' within 'DIF' folder (folder=NULL).
#' @param test Name of test.
#' @param DIFVar Name of variable to perform DIF analysis on.
#' @return Dataframe of delta, facility, etc..
#' @examples
#' file_its(test='RandD2', DIFVar='gender')

file_its <- function(folder=NULL, test, DIFVar){
    if (is.null(folder)) folder <- here::here('DIF', DIFVar)
    n_item <- N_item(folder=folder, test=test)
    file <- file_path_type(folder=folder, test=test, type='its')

    read_fwf(file,
             fwf_empty(file),
             skip=skip_its(file=file),
             n_max=n_item*2,
             show_col_types = FALSE)
}
