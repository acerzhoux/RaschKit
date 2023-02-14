#' section_export
#'
#' This function creates string of export specification for 'test.cqc' file
#' in 'input' folder. This is associated with test named 'test'.
#'
#' If poly_catgrs is not NULL, then each category's code is added into names
#' of output files. If poly_facet is TRUE, then string '_facet' is added into
#' names of output files of the facet model.
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

section_export <- function(poly_key, step, DIFVar,
                           poly_catgrs, poly_facet, poly_group){
    if(!is.null(poly_catgrs)) route <- paste0('%path%\\%test%_%', DIFVar, '%')
    else if (poly_facet) route <- '%path%\\%name%_facet'
    else if (poly_group) route <- '%path%\\%name%_group'
    else route <- '%path%\\%name%'

    if (poly_key & step){
        route <- str_c(route, '_', 'step')
    }

    c(paste0('export logfile                                   >> ',
             if (!is.null(poly_catgrs)) paste0('%path%\\%test%_%', DIFVar, '%_log.txt;\n')
             else if (poly_facet) '%path%\\%name%_facet_log.txt;\n'
             else '%path%\\%name%_log.txt;\n'),
      paste0('export parameters                                >> ', route, '_anc.txt;'),
      paste0('export reg_coefficients                          >> ', route, '_reg.txt;'),
      paste0('export covariance                                >> ', route, '_cov.txt;'),
      paste0('itanal !estimates=wle, format=summary            >> ', route, '_its.txt;'),
      paste0('itanal !estimates=wle, format=summary, filetype=excel >> ', route, '_its.xls;'),
      paste0('itanal !estimates=latent                         >> ', route, '_itn.txt;'),
      paste0('itanal !estimates=latent,filetype=excel          >> ', route, '_itn.xls;'),
      paste0('itanal !estimates=latent,format=export           >> ', route, '_opt.txt;'),
      paste0('itanal !estimates=latent,format=export,filetype=excel >> ', route, '_opt.xls;'),
      paste0('show !estimates=wle,Expanded=no,itemlabels=yes   >> ', route, '_shw.txt;'),
      paste0('show !estimates=wle,Expanded=no,itemlabels=yes,filetype=excel  >> ', route, '_shw.xls;'),
      if (poly_key & !step){
          c(paste0('show parameters !table=8                         >> ', route, '_del.txt;'),
            paste0('show parameters !table=8,filetype=excel          >> ', route, '_del.xls;'))
      },
      paste0('show parameters !table=7                         >> ', route, '_thr.txt;'),
      paste0('show parameters !table=7,filetype=excel          >> ', route, '_thr.xls;'),
      paste0('fit                                              >> ', route, '_res.txt;'),
      paste0('show cases !estimates=wle,filetype=text          >> ', route, '_cas.txt;'),
      paste0('show cases !estimates=wle,filetype=excel         >> ', route, '_cas.xls;'),
      paste0('put                                              >> ', route, '_compressed.CQS;'),
      'itanal ! matrixout=i;\n',
      if (is.null(DIFVar) | (!is.null(DIFVar) & is_true(poly_group))) {
          paste0('put ! compress=no                                >> ', route, '.CQS;\n')
      })
}
