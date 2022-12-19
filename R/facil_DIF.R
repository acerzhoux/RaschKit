#' facil_DIF
#'
#' This function extracts facilities of the two categories of the dichotomous 
#' DIF variable.
#'
#' @param folder Folder where .its file of the facet model on dichotomous DIF 
#' variable is located.
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param long_label Whether item labels are longer than 15 characters' fixed 
#' width. Default is FALSE.
#' @return Tibble of facilities of the two categories of the dichotomous DIF 
#' variable.
#' @examples
#' facil_DIF()
#' @export

facil_DIF <- function(folder, test, DIFVar, long_label=FALSE){
    N <- N_item(folder=folder, test=test)
    labels <- df_shw(folder=folder, test=test, long_label=long_label) %>%
        pull(item) %>%
        unique()

    file_its(folder=folder, test=test, DIFVar=DIFVar) %>%
        mutate(facility=str_sub(X1, -44, -37) %>%
                   as.numeric(),
               var=rep(1:2, N)) %>%
        select(var, facility) %>%
        mutate(item=rep(labels, each=2)) %>%
        pivot_wider(names_from=var,
                    values_from=facility) %>%
        mutate(dif=abs(`1`-`2`))
}
