#' DIF
#'
#' This function can perform DIF analysis on either dichotomous or polytomous
#' variables. The method to use depends on the 'method' argument.
#' 'method=chi_square' performs DIF analysis on a dichotomous variable.
#' Argument vars should be specified, e.g., c('girls', 'boys') whose order
#' corresponds to its categories' alphabetic/digital order in data.
#'  'method=Bonferroni' performs adjusted mean comparison  on polytomous DIF
#'  variable. Argument poly_catgrs should be specified, e.g., 1:5.
#'  'method=Facet' performs both facet and group modeling on polytomous
#'  DIF variable.

#' @param method One of 'chi_square', 'Bonferroni', or 'Facet'.
#' @param test Name of the test.
#' @param codes Vector of valid codes for item responses,
#' e.g., c(1, 2, 3, 4, 5, 6, 7, 8, 9).
#' @param pid_cols String of column numbers of person ID. Default is NULL.
#' @param resps_cols String of column numbers of responses, e.g., '20-30'.
#' @param regr_ls List of regressors. Element is column number in data.
#' Element name is regressor's name. Default is NULL.
#' @param anchor TRUE when anchor is to be done. Default is FALSE.
#' @param section_extr Extra sections to be added to 'test.cqc' file in
#' 'input' folder. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @param step TRUE if any item in the test has polytomous scoring. Default is FALSE.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' Default is NULL.
#' @param DIFVar_cols DIF variable's column number in data. Default is NULL.
#' @param poly_catgrs Vector of polytomous DIF variable's categories. Default is NULL.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable.
#' Default is FALSE.
#' @param poly_group TRUE if model is run per group. Default is FALSE.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order corresponds
#' to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests. Default is 4.
#' @param design_effect Value to adjust errors. Default is 1.
#' @param domain Name of the domain in the test, e.g., 'Literacy'. Default is NULL.
#' @param save_xlsx Whether to save summary file and plots. Default is TRUE
#' (one DIF variable).
#' @return Dataframe of students' ID, raw score, max test score, estimate,
#' and standard error.
#' @param iter TRUE to iteratively remove DIF items. Default is TRUE.
#' @param pweight Variable name of person weights in response dataframe. Should
#' be specified if weight is used for modeling. Default is NULL.
#' @param pw_cols String of column numbers of case weight, e.g., '5-15'.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @examples
#' # Not run
#' # DIF(vars=c('girls', 'boys'), test='literacy', codes=c(1:4, 9),
#' # pid_cols='1-12', resps_cols='19-48', DIFVar='gender', DIFVar_cols='14',
#' # regr_ls=list(nation='13', age='15-16'), quick=TRUE)
#' # DIF(poly_catgrs=1:5, test='literacy', codes=c(1:4, 9), pid_cols='1-12',
#' # resps_cols='19-48', DIFVar='nation', DIFVar_cols='13',
#' # regr_ls=list(G2='17', age='15-16'), quick=TRUE)
#' # DIF(poly_facet=TRUE, test='literacy', codes=c(1:4, 9), pid_cols='1-12',
#' # resps_cols='19-48', DIFVar='nation', DIFVar_cols='13',
#' # regr_ls=list(G2='17', age='15-16'), quick=TRUE)
#' @export

DIF <- function(method=c('chi_square', 'Bonferroni', 'Facet'), test, keyDf, #### CQC #####
                codes, pid_cols=NULL, resps_cols, regr_ls=NULL, anchor=FALSE,
                section_extr=NULL, poly_key=FALSE, quick=TRUE, step=FALSE,
                DIFVar=NULL, DIFVar_cols=NULL, poly_catgrs=NULL, ##### DIF part #####
                poly_facet=FALSE, poly_group=FALSE, vars=NULL, p_cut=0.05,
                DIF_cut=0.5, DIF_std_cut=4, design_effect=1, domain=NULL,
                save_xlsx=TRUE, iter=TRUE, pweight=NULL, pw_cols=NULL){
  # check inputs
  if (length(method)!=1 || !(method %in% c('chi_square', 'Bonferroni', 'Facet'))) {
    stop('Please set \'method\' as one of \'chi_square\', \'Bonferroni\', or \'Facet\'.')
  }

  arg_cqc <- list(test=test, run=NULL, keyDf=keyDf, ####CQC
    codes=codes, pid_cols=pid_cols, resps_cols=resps_cols,
    quick=quick, poly_key=poly_key,
    anchor=anchor, step=step, regr_ls=regr_ls, section_extr=section_extr,
    DIFVar=DIFVar, DIFVar_cols=DIFVar_cols, poly_catgrs=poly_catgrs,
    poly_facet=poly_facet, poly_group=poly_group,
    pweight=pweight, pw_cols=pw_cols)
  arg_DIF <- list(DIFVar=DIFVar, test=test, p_cut=p_cut, step=step)

  if (method=='chi_square'){
    if (is.null(vars)) stop('Please set \'vars\' as name vector of DIF variable\'s two categories.
      Order should correspond to their alphebetic/numerical orders in data coding.')

    cat('Running ConQuest for facet model...\n')
    do.call(lab_cqc, arg_cqc)

    cat('Performing', 'chi_square tests with facet model results',
      if (iter) 'iteratively' else 'once and for all', '...\n')
    do.call(
      DIF_dich_its_shw,
      append(
        arg_DIF,
        list(
          vars=vars,
          DIF_cut=DIF_cut, DIF_std_cut=DIF_std_cut,
          design_effect=design_effect, save_xlsx=save_xlsx,
          iter=iter, quick=quick
        )
      )
    )
  }

  if (method=='Bonferroni'){
    if (is.null(poly_catgrs)) stop('Please set \'poly_catgrs\' as vector of DIF variable\'s categories\' integer code.')

    cat('Running ConQuest: Item calibration for each subgroup...\n')
    do.call(lab_cqc, arg_cqc)

    cat('Performing Bonferroni adjusted comparison...\n')
    do.call(
      DIF_poly_shw,
      append(arg_DIF, list(labels=NULL, domain=domain))
    )
  }

  if (method=='Facet'){
    # facet model
    cat('Running ConQuest for facet model...\n')
    arg_cqc[['poly_facet']] <- TRUE
    do.call(lab_cqc, arg_cqc)

    # group model
    cat('Running ConQuest for group model...\n')
    arg_cqc[['poly_group']] <- TRUE
    arg_cqc[['poly_facet']] <- FALSE
    arg_cqc[['quick']] <- FALSE
    do.call(lab_cqc, arg_cqc)

    # summarise and plot results
    cat('Summarising results from facet model...\n')
    results <- df_shw_Term3(DIFVar, test)
      # mutate(!!sym(DIFVar) := as.integer(!!sym(DIFVar)))

    ls_save <- list(
      filter(results[1:13], Flag==1),
      results
    )
    names(ls_save) <- c('summary', test)

    # add format
    file <- file.path('DIF', paste0(DIFVar, '_', test, '_Facet.xlsx'))
    add_format()[['DIFFacet']](
      ls_save,
      file.path(DIFVar, paste0(test, '_Group.pdf')),
      file
    )

    cat('Plotting results from group model...\n')
    plot_DIF_group(test=test, DIFVar=DIFVar)

    # point users to files of varying purposes
    writeLines(c(
      paste0('\n========= Output Files =========\n'),
      paste0(toupper(DIFVar), ' DIF analysis for ', test, ' (Facet model & group model):'),
      paste0('\tSummary:\t\t', 'DIF/', DIFVar, '_', test, '_Facet.xlsx'),
      paste0('\tExpected Score Curves:\t', 'DIF/', DIFVar, '/', test, '_Group.pdf')
    ))
  }
}
