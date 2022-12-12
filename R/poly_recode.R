#' poly_recode
#'
#' This function down ranks non-continuous polytomous item response vectors in a dataframe.
#'
#' @param data Dataframe with pid, covariables (e.g,, DIF variable), and responses. Default is NULL where Excel file with name 'test' in 'data' folder is used.
#' @param keys Vector of keys in the test. Default is NULL.
#' @param n_cov Number of covariates before responses.
#' @param miss_code Missing codes. Default is c('r','R','m','M','9','x','X','.','',' ',NA).
#' @export

poly_recode <- function(keys, data, n_cov, miss_code=c('r','R','m','M','9','x','X','.','',' ',NA)){
    id_poly <- which(keys$Max_score > 1) + n_cov
    data %>%
        modify_at(id_poly, ~poly_recode_one(.x, miss_code = miss_code))
}
