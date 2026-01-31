#' section_export
#'
#' This function creates string of export specification for 'test.cqc' file
#' in 'input' folder. This is associated with test named 'test'.
#'
#' If poly_catgrs is not NULL, then each category's code is added into names
#' of output files. If poly_facet is TRUE, then string '_facet' is added into
#' names of output files of the facet model. If poly_group is TRUE, then string
#' '_group' is added into names of output files of the group model.
#'
#' @param poly_key TRUE if the key of any item has polytomous scoring.
#' Default is FALSE.
#' @param step TRUE if a three-way interaction term of item, step, and DIF
#' variable is used in facet modeling.
#' @param DIFVar Name of DIF variable.
#' @param poly_catgrs Vector of categories of the polytomous DIF variable 'DIFVar'.
#' @param poly_facet TRUE if facet model is to be run on a polytomous DIF variable.
#' @param poly_group TRUE if model is run per group. Default is FALSE.
#' @return String of characters used in export section of 'test.cqc' file
#' in 'input' folder.
#' @examples
#' section_export()
#' @export

section_export <- function(poly_key, step, DIFVar, poly_catgrs, poly_facet, poly_group){
  if(!is.null(poly_catgrs)) p <- paste0('%path%\\%test%_%', DIFVar, '%')
  else if (poly_facet) p <- '%path%\\%name%_facet'
  else if (poly_group) p <- '%path%\\%name%_group'
  else p <- '%path%\\%name%'

  if (poly_key & step) p <- paste0(p, '_step')

  c(
    paste0('export logfile                                                 >> ', p, '_log.txt;'),
    paste0('export parameters                                              >> ', p, '_anc.txt;'),
    paste0('export reg_coefficients                                        >> ', p, '_reg.txt;'),
    paste0('export covariance                                              >> ', p, '_cov.txt;'),
    paste0('itanal !estimates=wle,format=summary                           >> ', p, '_its.txt;'),
    paste0('itanal !estimates=wle,format=summary,filetype=excel            >> ', p, '_its.xls;'),
    paste0('itanal !estimates=latent                                       >> ', p, '_itn.txt;'),
    if (is.null(DIFVar)){
      paste0('itanal !estimates=latent,filetype=excel                        >> ', p, '_itn.xls;')
    },
    paste0('itanal !estimates=latent,format=export                         >> ', p, '_opt.txt;'),
    paste0('itanal !estimates=latent,format=export,filetype=excel          >> ', p, '_opt.xls;'),
    paste0('show !estimates=wle,Expanded=no,itemlabels=yes                 >> ', p, '_shw.txt;'),
    paste0('show !estimates=wle,Expanded=no,itemlabels=yes,filetype=excel  >> ', p, '_shw.xls;'),
    if (poly_key & !step){
      c(paste0('show parameters !table=8                                       >> ', p, '_del.txt;'),
      paste0('show parameters !table=8,filetype=excel                        >> ', p, '_del.xls;'))
    },
    paste0('show parameters !table=7                                       >> ', p, '_thr.txt;'),
    paste0('show parameters !table=7,filetype=excel                        >> ', p, '_thr.xls;'),
    paste0('fit                                                            >> ', p, '_res.txt;'),
    paste0('show cases !estimates=wle,filetype=excel                       >> ', p, '_cas.xls;'),
    if (is.null(DIFVar)) {
      paste0('put                                                            >> ', p, '_compressed.CQS;')
    },
    'itanal ! matrixout=i;\n',
    # if (is.null(DIFVar) | (!is.null(DIFVar) & is_true(poly_group))) {
      paste0('put ! compress=no                                              >> ', p, '.CQS;\n')
    # }
  )
}
