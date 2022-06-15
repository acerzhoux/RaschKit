#' section_data
#'
#' This function creates string of data specification for 'test.cqc' file in 'input' folder. This is associated with test named 'test'.
#'
#' Variable names and variable column numbers are added when related arguments are not NULL. Those addable variables are pid_cols (person ID), DIFVar (DIF variable), run_ls (data filters), and regr_ls (regressors). Also, models can be run per group if poly_group is TRUE.
#'
#' @param path_df Path of data to import in 'data' folder.
#' @param resps_cols Column numbers of responses.
#' @param pid_cols Column numbers of person ID.
#' @param run_ls List of data filters. Element is column number in data. Element name is filter variable's name.
#' @param regr_ls List of regressors. Element is column number in data. Element name is regressor name.
#' @param path_lab Route of label file in 'input' folder.
#' @param DIFVar Name of DIF variable.
#' @param DIFVar_cols DIF variable's column number in data.
#' @param poly_group TRUE if model is run per group.
#' @return String of characters used in export section of 'test.cqc' file in 'input' folder.
#' @examples
#' section_data()

section_data <- function(path_df, resps_cols, pid_cols, run_ls, regr_ls,
                         path_lab, DIFVar, DIFVar_cols, poly_group){
    c(paste0('data ', path_df, ';\n'),
      paste('format responses', resps_cols,
            if (!is.null(pid_cols)) paste('pid', pid_cols),
            if (!is.null(DIFVar)) paste(DIFVar, DIFVar_cols),
            if (!is.null(run_ls)) paste(paste(names(run_ls), run_ls), collapse=' '),
            if (!is.null(regr_ls)) paste(paste(names(regr_ls), regr_ls), collapse=' '),
            ';\n'),
      if (poly_group) paste0('group ', DIFVar, ';\n'),
      paste0('labels                                           << ', path_lab, ';\n'))
}
