#' est_cas
#'
#' This function extracts ability estimation from 'test.cas' file.
#' This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.cas' file is located. Default is 'output' folder.
#' @param test Name of test.
#' @return Dataframe with variables of 'Pid', 'ScoreRaw', 'ScoreMax', 'Est', and 'SE'.
#' @examples
#' # Not run
#' # est_cas(test='HUM_2')
#' @export

est_cas <- function(folder=here::here('output'), test){
    est_cas <- read.table(file.path(folder, paste0(test, '.cas'))) %>%
        `colnames<-`(c('ord','Pid','ScoreRaw','ScoreMax','Est','SE')) %>%
        select(-ord) %>%
        modify_at(4:5, ~round(., 3)) %>%
        as_tibble()

    est_cas %>%
        writexl::write_xlsx(here::here('results', paste0('estimates_', test, '.xlsx')))

    est_cas
}
