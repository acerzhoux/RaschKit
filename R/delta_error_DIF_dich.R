#' delta_error_DIF_dich
#'
#' This function extracts error statistics for two categories of dichotomous 
#' DIF variable.
#'
#' @param folder Folder where .shw file is located with delta estimates for 
#' interaction term between dichotomous DIF variable and item. Default is 'DIF' 
#' folder in working directory.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed 
#' width. Default is FALSE.
#' @return Dataframe of three variables of 'item', 'error.x', 'error.y'.
#' @examples
#' delta_error_DIF_dich(folder=folder, test='RandD2')
#' delta_error_DIF_dich(folder=folder, test='RandD2', long_label=TRUE)
#' @export

delta_error_DIF_dich <- function(folder, test, long_label=FALSE){
    n_item <- N_item(folder=folder, test=test)

    items_shw <- read_fwf(file_path_type(folder=folder, test=test, type='shw'),
             fwf_cols(iNum=c(2, 5),
                      item=c(6, 16),
                      var=c(17, 19),
                      error=c(46, 50)),
             skip=(term3_L2(folder=folder, test=test)+5),
             n_max=n_item * 2,
             show_col_types = FALSE) %>%
        pivot_wider(names_from=var,
                    values_from=error) %>%
        `colnames<-`(c('iNum', 'item', 'error.x', 'error.y'))

    if (long_label){
        labs <- read.table(here::here('data', paste0(test, '_Labels.txt'))) %>%
            mutate(iNum=as.numeric(rownames(.))) %>%
            rename(Label=V1)
        items_shw %>%
            left_join(labs, by='iNum') %>%
            select(-iNum, -item, item=Label)
    } else {
        items_shw %>%
            select(-iNum)
    }
}
