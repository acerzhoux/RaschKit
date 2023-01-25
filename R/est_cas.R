#' est_cas
#'
#' This function extracts ability estimation from 'test.cas' file.
#' This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @return Dataframe with variables of 'Pid', 'ScoreRaw', 'ScoreMax', 'Est', and 'SE'.
#' @examples
#' # Not run
#' # est_cas(test='FPA')
#' @export

est_cas <- function(test){
    est_cas <- read.table(paste0('output/', test, '_cas.txt')) |>
        `colnames<-`(
            c('ord', 'Pid', 'ScoreRaw', 'ScoreMax', 'Est', 'SE')
        ) |>
        select(-ord) |>
        modify_at(4:5, ~round(., 3)) |>
        as_tibble()

    est_cas |>
        writexl::write_xlsx(paste0('results/', 'est_', test, '.xlsx'))

    est_cas
}
