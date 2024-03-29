#' lab_cqc
#'
#' This function creates folders in the working directory, and creates
#' 'test.lab' file and 'test.cqc' file in 'input' folder.
#'
#' For DIF analysis on dichotomous variables, you need specify 'DIFVar', 'DIFVar_cols'.
#' For DIF analysis on polytomous variables, there are two methods. Method 1
#' compares each category's delta estimates with the mean of all subgroups.
#' Specified arguments are 'DIFVar', 'DIFVar_cols', 'DIFVar_cols'.
#' Method 2 runs two models. The first model does calibrations on each subgroup
#' separately where you need to specify arguments of 'DIFVar', 'DIFVar_cols',
#' and 'poly_group'. The other model runs a facet model with an interaction term
#' between group and item where you need to specify arguments of 'DIFVar',
#' 'DIFVar_cols', and 'poly_facet'.
#'
#' @param test Name of the test.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score'.
#' @param run Vector of specific categories of variables to select from
#' 'test_Data.txt' in 'data' folder, e.g., c('English2', 3, 1). This corresponds
#' to the previous argument `run_ls`. Default is NULL.
#' @param resps_cols String of column numbers of responses, e.g., '20-30'.
#' @param pid_cols String of column numbers of person ID. Default is NULL.
#' @param run_ls List of data filters. Element is column number in data.
#' Element name is filter variable's name. Default is NULL.
#' @param regr_ls List of regressors. Element is column number in data.
#' Element name is regressor's name. Default is NULL.
#' @param codes Vector of valid codes for item responses,
#' e.g., c(1, 2, 3, 4, 5, 6, 7, 8, 9).
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in
#' 'input' folder. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param quick TRUE if quick estimation is preferred. Default is FALSE.
#' @param step TRUE if any item in the test has polytomous scoring. Default is FALSE.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' Default is NULL.
#' @param DIFVar_cols DIF variable's column number in data. Default is NULL.
#' @param poly_catgrs Vector of polytomous DIF variable's categories. Default is NULL.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable.
#' Default is FALSE.
#' @param poly_group TRUE if model is run per group. Default is FALSE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @param pw_cols String of column numbers of case weight, e.g., '5-15'.
#' @param strRec String of recoding for ConQuest control file. Default is NULL.
#' @return 'test.cqc' file in 'input' folder.
#' @examples
#' lab_cqc()
#' @export

lab_cqc <- function(test, keyDf, run=NULL, run_ls=NULL,
                   codes, pid_cols=NULL, resps_cols, quick=FALSE,
                   poly_key=FALSE, anchor=FALSE, section_extr=NULL,
                   step=FALSE, regr_ls=NULL, DIFVar=NULL, DIFVar_cols=NULL, #dich & poly
                   poly_catgrs=NULL, #dich, poly
                   poly_facet=FALSE, poly_group=FALSE, #poly: facet
                   pweight=NULL, pw_cols=NULL, strRec=NULL){
  # run_ls: list(domain='3-11', grade='12-13', flag='36')
  #   for testform; keyDf, labels differ; put in 'Keepcases'
  # run: c('English2', 3, 1); used with `run_ls`
  # poly_catgrs: poly DIF; keyDf, labels same for 'Keepcases'
  # DIFVar: Lowercase to run 'conquestr'

  # create folders
  create_folders(DIFVar=DIFVar)

  # create label file
  create_lab(test=test) # Item label file

  # ##### create control file # #####

  # specify paths
  path_output <- if (is.null(DIFVar)) 'Output' else paste0('DIF/', DIFVar)
  path_lab <- paste0('input/', test, '_lab.txt')

  if (is.null(DIFVar)){
    path_df <- paste0('data/',
            if (is.null(run_ls)) paste0(test, '_Data.txt') else
              paste0('Data.txt'))
  } else {
    path_df <- paste0('data/', test, '_', DIFVar, '.txt')
  }

  # compose CQC string
  cqc <- c(
    section_intro(test, run_ls, path_output, DIFVar, poly_catgrs),
    section_data(path_df, resps_cols, pid_cols, run_ls, regr_ls,
                 path_lab, DIFVar, DIFVar_cols, poly_group, pweight, pw_cols),
    section_keys(keyDf),
    section_specs(anchor, test, DIFVar, poly_catgrs, quick),
    if (!is.null(section_extr)) section_extr,
    if (poly_key) strRec,
    section_model(run_ls, run, regr_ls, codes, poly_key, DIFVar, step, poly_group),
    section_estimate(quick, poly_key),
    section_export(poly_key, step, DIFVar, poly_catgrs, poly_facet, poly_group),
    'reset;',
    if (!is.null(poly_catgrs)) 'enddo;',
    'quit;'
  ) |>
  str_replace_all(c(' ;'=';', '  ;'=';', '   ;'=';', '  ;'=';', '   ;'=';'))

  # write CQC into folder
  cqc_path <-  paste0(
    'input/',
    if (is.null(DIFVar)) paste0(test, '.cqc')
    else if (poly_facet) paste0(DIFVar, '_', test, '_facet.cqc')
    else if (poly_group) paste0(DIFVar, '_', test, '_group.cqc')
    else if (poly_key & step) paste0(DIFVar, '_', test, '_step.cqc')
    else paste0(DIFVar, '_', test, '.cqc')
  )
  writeLines(cqc, cqc_path)

  # run CQC
  conquestr::ConQuestCall(
    cqc = cqc_path,
    cqExe = file.path('C:', 'Program Files', 'ACER ConQuest', 'ConQuestConsole.exe'),
    stdout = NULL
  )
}
