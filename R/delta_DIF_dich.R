#' delta_DIF_dich
#'
#' This function extracts delta statistics for two categories of dichotomous DIF variable.
#'
#' @param folder Folder where subgroups' calibration results are located. 
#' Default is the subfolder with name of argument 'DIFVar' within 'DIF' folder 
#' (folder=NULL).
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return Dataframe of variables of 'delta.x', 'delta.y'.
#' @examples
#' delta_DIF_dich(test='AHU', DIFVar='ATSI', quick=F)
#' @export

delta_DIF_dich <- function (folder = NULL, test, DIFVar, quick = TRUE){
    if (is.null(folder)) folder <- here::here("DIF", DIFVar)
    n_item <- N_item(folder = folder, test = test)
    delta_its <- file_its(folder = folder,
                          test = test,
                          DIFVar = DIFVar) %>%
        mutate(delta = as.numeric(str_sub(X1, -8, -1)),
               var = rep(1:2, n_item)) %>%
        select(var, delta)
    if (quick) {
        delta_its <- delta_its %>%
            nest_by(var) %>%
            mutate(m = mean(data$delta, na.rm=TRUE)) %>%
            unnest(data) %>%
            mutate(delta = delta - m) %>%
            select(-m) %>%
            ungroup()
    }
    delta_its %>%
        filter(var == 1) %>%
        dplyr::rename(delta.x = delta) %>%
        cbind(delta_its %>%
                  filter(var == 2) %>%
                  dplyr::rename(delta.y = delta)) %>%
        select(-var)
}
