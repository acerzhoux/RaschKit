#' facil_DIF
#'
#' This function extracts facilities of the two categories of the dichotomous
#' DIF variable.
#'
#' @param test Name of test.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @return Tibble of facilities of the two categories of the dichotomous DIF
#' variable.
#' @examples
#' facil_DIF(test=test, DIFVar='gender')
#' @export

facil_DIF <- function(test, DIFVar=NULL){
    folder <- paste0('DIF/', DIFVar)
    labels <- df_shw(folder, test) |>
        pull(item) |>
        unique()

    file_its(folder, test, DIFVar) |>
        mutate(
            facility = str_sub(X1, -44, -37) |> as.numeric(),
            var = rep(1:2, N_item(folder, test))
        ) |>
        dplyr::select(var, facility) |>
        mutate(item = rep(labels, each=2)) |>
        pivot_wider(
            names_from = var,
            values_from = facility
        ) |>
        mutate(dif = abs(`1`-`2`))
}
