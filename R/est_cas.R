#' est_cas
#'
#' This function extracts ability estimation from 'test.cas' file. This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.cas' file is located. Default is 'output' folder.
#' @param test Name of test.
#' @param sav_xlsx TRUE if an Excel file is desired. Default is TRUE.
#' @return Dataframe with variables of 'Pid', 'ScoreRaw', 'ScoreMax', 'Est', and 'SE'.
#' @examples
#' est_cas(test='HUM_2')
#' @export

# extract case WLE
est_cas <- function(folder=here::here('output'), test, sav_xlsx=TRUE){
    file <- file.path(folder, paste0(test, '.cas'))
    ests <- read.table(file) %>%
        `colnames<-`(c('ord','Pid','ScoreRaw','ScoreMax','Est','SE')) %>%
        select(-ord) %>%
        modify_at(4:5, ~round(., 3))

    if (sav_xlsx) {
        ests %>%
            writexl::write_xlsx(here::here('results', paste0('estimates_', test, '.xlsx')))
    }
    ests
}
