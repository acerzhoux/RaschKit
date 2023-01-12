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
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of the test.
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
#' @param delete Vector of item order number(s) to be removed from the test,
#' e.g., c(2, 3, 45, 46). Default is NULL.
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in
#' 'input' folder. Default is NULL.
#' @param dbl_key TRUE if any item has polytomous scoring. Default is NULL.
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
#' @return 'test.cqc' file in 'input' folder.
#' @examples
#' lab_cqc()
#' @export

lab_cqc <- function(wd=here::here(), test, run=NULL, run_ls=NULL,
                    codes, pid_cols=NULL, resps_cols, quick=FALSE, delete=NULL,
                    dbl_key=NULL, poly_key=FALSE, anchor=FALSE, section_extr=NULL,
                    step=FALSE, regr_ls=NULL,
                    DIFVar=NULL, DIFVar_cols=NULL, #dich & poly
                    poly_catgrs=NULL, #dich, poly
                    poly_facet=FALSE, poly_group=FALSE, #poly: facet
                    pweight=NULL, pw_cols=NULL){
    # create folders
    create_folders(wd=wd, DIFVar=DIFVar)

    # create label file
    create_lab(wd=wd, test=test) # Item label file

    # create control file
    create_cqc(wd=wd, test=test, run=run, resps_cols=resps_cols, pid_cols=pid_cols,
               run_ls=run_ls, anchor=anchor, section_extr=section_extr,
               regr_ls=regr_ls, codes=codes, delete=delete, dbl_key=dbl_key,
               poly_key=poly_key, quick=quick, step=step, DIFVar=DIFVar,
               DIFVar_cols=DIFVar_cols, poly_catgrs=poly_catgrs,
               poly_facet=poly_facet, poly_group=poly_group,
               pweight=pweight, pw_cols=pw_cols)
}
