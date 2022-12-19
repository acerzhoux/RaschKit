#' fit_stats
#'
#' This function extracts fit statistics from 'test.shw' file. This is 
#' associated with test named 'test'.
#'
#' @param folder Folder where 'test.shw' file is located. Default is 'output' folder.
#' @param test Name of test.
#' @examples
#' fit_stats(test='elana_poly_score')
#' @export

fit_stats <- function(folder=here::here('output'), test){
    n_item <- N_item(folder=folder, test=test)

    read_fwf(file_path_type(folder=folder, test=test, type='shw'),
             fwf_cols(qOrder=c(1, 5),
                      error=c(29, 38),
                      MNSQ_uw=c(40, 44),
                      MNSQ_w=c(66, 70),
                      CI_L=c(72, 76),
                      CI_U=c(78, 82),
                      T=c(84, 88)),
             skip=term_L2(folder=folder, test=test)+5,
             n_max=n_item,
             show_col_types = FALSE) %>%
        mutate(qOrder=as.character(qOrder))
}
