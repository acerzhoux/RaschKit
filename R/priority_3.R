#' priority_3
#'
#' This function checks response option point biserial (Pt Bis) values in
#' 'xxx_itn.txt' file in 'output' folder and flag items with any distractor of
#' positive Pt Bis. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @return Dataframe of flags and item order.
#' @examples
#' priority_3(test='FPA')
#' @export

priority_3 <- function(test){
    tibble(
        priority_3 = itn_ls(test) |>
            map(~dplyr::filter(.x, Score == 0)) |>
            map_lgl(~any(.x$PtBis > 0)),
        qOrder = 1:length(priority_3)
    )
}
