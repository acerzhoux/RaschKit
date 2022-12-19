#' df_shw
#'
#' This function extracts item delta and error estimates from .shw file. This 
#' is associated with test named 'test'. If any item label is longer than 15 
#' characters' fixed width, use long_label=TRUE to read complete labels from 
#' 'test_Labels.txt' file in 'data' folder.
#'
#' @param folder Folder where .shw file is located.
#' @param test Name of test.
#' @param long_label Whether item labels are longer than 15 characters' fixed 
#' width. Default is FALSE.
#' @return Dataframe of item name, delta, and error.
#' @examples
#' df_shw(test='racp1', long_label=TRUE)
#' df_shw(test='racp1')
#' @export

df_shw <- function(folder=here::here('output'), test, long_label=FALSE){
    items_shw <- file_path_type(folder=folder, test=test, type='shw') %>%
        read_fwf(fwf_cols(iNum=c(2, 5),
                          item=c(6, 20),
                          delta=c(21, 27),
                          error=c(31, 35)),
                 skip=term_L2(folder=folder, test=test)+5,
                 n_max=N_item(folder=folder, test=test),
                 show_col_types = FALSE)

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
