#' itn_ls
#'
#' This function extract item analysis results for each item from .itn file 
#' and put them into a list. This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 
#' 'test'. Default is 'output' folder in working directory.
#' @param test Name of test.
#' @export

itn_ls <- function(folder=here::here('output'), test){
    itn_str <- list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.itn')) %>%
        readLines()
    a <- itn_str %>% str_detect('Label    ') %>% which()
    b <- itn_str %>% str_detect('==========') %>%
        which() %>% .[-c(1,2, N_item2(folder=folder, test=test)+3)]
    nn_opt <- (b-1) - (a+2) + 1

    map2(a+2, b-1, ~itn_str[.x:.y]) %>%
        map2(., nn_opt,
             ~read_fwf(I(.x), fwf_cols(Label=c(2, 6),
                                       Score=c(11, 15),
                                       Count=c(21, 28),
                                       Percent=c(29, 36),
                                       PtBis=c(39, 44),
                                       Tau=c(47,52),
                                       P=c(54, 57),
                                       PV1=c(59, 67),
                                       SD=c(68, 75)),
                       skip=0,
                       n_max=.y,
                       show_col_types = FALSE))
}
