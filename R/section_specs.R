#' section_specs
#'
#' This function specifies the section of estimation details in 'test.cqc' 
#' file in 'input' folder. This is associated with test named 'test'.
#'
#' When anchoring is to be done, the import route of the anchor file is 
#' designated and item constraint is specified as 'constraints=none'. 
#' When DIF analysis is to be done on a polytomous DIF variable, it is 
#' specified that some category of the DIF variable is to be selected from 
#' among all of its categories.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param anchor TRUE when anchor is to be done.
#' @param test Name of the test.
#' @param DIFVar Name of the DIF variable.
#' @param poly_catgrs Vector of categories of the polytomous DIF variable 'DIFVar'.
#' @param quick TRUE when testing.
#' @return String of characters used in estimation section of 'test.cqc' 
#' file in 'input' folder.
#' @examples
#' section_specs()
#' @export

section_specs <- function(wd, anchor, test, DIFVar, poly_catgrs, quick){
    if (anchor) anc_path <- file.path(wd, 'Input', paste0(test, '.anc'))

    c(paste0('set addextension=no, keeplastest=yes, iterlimit=1000, ',
      if(anchor) {
        paste0('constraints=none;\nimport anchor_parameters <<', anc_path, ';\n')
      } else {
        if (quick){
          'lconstraints=cases;\n'
        } else {
          'constraints=items;\n'
        }
      }),

      if (!is.null(poly_catgrs)) paste0('Keepcases %', DIFVar, '%! ', DIFVar, ';\n'))
}
