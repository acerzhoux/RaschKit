#' keys_poly_into_Data
#'
#' This function saves raw key file 'test_Key.txt' into 'data' folder. Later, this saved file is used to produce the key section of 'test.cqc' file in 'input' folder. This is associated with polytomous item test named 'test'.

#' @param test Name of test.
#' @param score_max Maximum score of single polytomous items in the test.
#' @param N_item Number of items in the test.
#' @examples
#' keys_poly_into_Data()
#' @export

keys_poly_into_Data <- function(test, score_max, N_item){
    matrix(rep(1:score_max, n_test), ncol=n_test, byrow = FALSE) %>%
        as.data.frame() %>%
        gdata::write.fwf(
            here::here('data', paste0(test, '_Key.txt')),
            justify="left",
            colnames = FALSE, sep = "")
}
