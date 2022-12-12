#' create_cqc
#'
#' This function creates 'test.cqc' file in 'input' folder. This is associated with test named 'test'.
#'
#' For DIF analysis on dichotomous variables, you need specify 'DIFVar', 'DIFVar_cols'.
#' For DIF analysis on polytomous variables, there are two methods. Method 1 compares each category's delta estimates with the mean of all subgroups. Specified arguments are 'DIFVar', 'DIFVar_cols', 'DIFVar_cols'. Method 2 runs two models. The first model does calibrations on each subgroup separately where you need to specify arguments of 'DIFVar', 'DIFVar_cols', and 'poly_group'. The other model runs a facet model with an interaction term between group and item where you need to specify arguments of 'DIFVar', 'DIFVar_cols', and 'poly_facet'.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of the test.
#' @param run Vector of specific categories of variables to select from 'test_Data.txt' in 'data' folder, e.g., c('English2', 3, 1). This corresponds to the previous argument `run_ls`.
#' @param resps_cols Column numbers of responses.
#' @param pid_cols Column numbers of person ID.
#' @param run_ls List of data filters. Element is column number in data. Element name is filter variable's name. This argument is useful for data including more than one test forms each of which has respective keys and labels. The elements will be used by 'Keepcases' to filter a test form.
#' @param regr_ls List of regressors. Element is column number in data. Element name is regressor's name.
#' @param codes Vector of valid codes for item responses, e.g., c(1, 2, 3, 4, 5, 6, 7, 8, 9).
#' @param delete Vector of item order number to be removed from the test, e.g., c(2, 3, 45, 46).
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in 'input' folder.
#' @param dbl_key TRUE if any item has polytomous scoring. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param quick TRUE if quick estimation is preferred. Default is FALSE.
#' @param step TRUE if any item in the test has polytomous scoring. Default is FALSE.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' @param DIFVar_cols DIF variable's column number in data.
#' @param poly_catgrs Vector of polytomous DIF variable's categories.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable. Default is FALSE.
#' @param poly_group TRUE if model is run per group. Default is FALSE.
#' @examples
#' create_cqc()
#' @export

create_cqc <- function(wd=here::here(), test, run, resps_cols, pid_cols, run_ls=NULL,
                       regr_ls, codes, delete, anchor=FALSE, section_extr=NULL,
                       dbl_key=NULL, poly_key=FALSE, quick=FALSE, step=FALSE,
                       DIFVar=NULL, DIFVar_cols, poly_catgrs=NULL,
                       poly_facet=FALSE, poly_group=FALSE){
    # run_ls: list(domain='3-11', grade='12-13', flag='36')
    #   for testform; keys, labels differ; put in 'Keepcases'
    # run: c('English2', 3, 1); used with `run_ls`
    # poly_catgrs: poly DIF; keys, labels same for 'Keepcases'
    # DIFVar: Lowercase to run 'conquestr'

    # specify paths
    folder_df <- file.path(wd, 'Data')
    path_output <- file.path(wd, if (is.null(DIFVar)) 'Output' else paste0('DIF/', DIFVar))
    path_lab <-  file.path(wd, 'Input', paste0(test, '.lab'))

    if (is.null(DIFVar)){
        path_df <- file.path(folder_df,
                             if (is.null(run_ls)) paste0(test, '_Data.txt') else
                                 paste0('Data.txt'))
    } else {
        path_df <- file.path(folder_df, paste0(test, '_', DIFVar, '.txt'))
    }

    # modify if last response col(s) has no data
    if (length(delete)>0) {
        resps_cols <- resps_modify(folder=folder_df, test=test,
                                   resps_cols=resps_cols, delete=delete)}
    # compose CQC string
    cqc <- c(section_intro(path_output=path_output, test=test,  run_ls=run_ls,
                           DIFVar=DIFVar, poly_catgrs=poly_catgrs),
             section_data(path_df=path_df, resps_cols=resps_cols, pid_cols=pid_cols,
                          run_ls=run_ls, regr_ls=regr_ls, path_lab=path_lab,
                          DIFVar=DIFVar, DIFVar_cols=DIFVar_cols, poly_group=poly_group),
             section_keys(folder=folder_df, test=test, dbl_key=dbl_key,
                          poly_key=poly_key, delete=delete),
             section_specs(anchor=anchor, wd=wd, test=test, DIFVar=DIFVar,
                           poly_catgrs=poly_catgrs, quick=quick),
             if (!is.null(section_extr)) section_extr,
             section_model(run_ls=run_ls, run=run, regr_ls=regr_ls, codes=codes,
                           poly_key=poly_key, DIFVar=DIFVar, step=step),
             section_estimate(quick=quick, poly_key=poly_key),
             section_export(poly_key=poly_key, step=step, DIFVar=DIFVar,
                            poly_catgrs=poly_catgrs,
                            poly_facet=poly_facet, poly_group=poly_group),
             'reset;',
             if (!is.null(poly_catgrs)) 'enddo;',
             'quit;') %>%
        str_replace_all(c(' ;'=';', '  ;'=';', '   ;'=';', '    ;'=';', '     ;'=';'))

    # write CQC into folder
    cqc_path <-  file.path(wd, 'input',
                           if (is.null(DIFVar)) paste0(test, '.cqc')
                           else if (poly_facet) paste0(DIFVar, '_', test, '_facet.cqc')
                           else if (poly_group) paste0(DIFVar, '_', test, '_group.cqc')
                           else if (poly_key & step) paste0(DIFVar, '_', test, '_step.cqc')
                           else paste0(DIFVar, '_', test, '.cqc'))
    writeLines(cqc, cqc_path)

    # run CQC
    conquestr::ConQuestCall(file.path('C:', 'Program Files', 'ACER ConQuest', 'ConQuestConsole.exe'),
        cqc = cqc_path)
}
