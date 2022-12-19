#' freq_all_scores
#'
#' This function reads in a vector of raw scores and return a dataframe of 
#' raw scores, frequencies, cumulative frequencies, and cumulative percentiles.
#'
#' This function uses 0 as minimum score and allows users to input a maximum 
#' score of the test. Floor is used to obtain whole numbers for cumulative percentiles.
#'
#' @param vec Vector of all scores in the test.
#' @param max_lev Maximum score in the test.
#' @return Dataframe with raw scores, frequencies, cumulative frequencies, 
#' and cumulative percentiles.
#' @examples
#' freq_all_scores(vec=sample(c(0:40), 5000, replace = TRUE), max_lev=42)
#' @export

freq_all_scores <- function(vec, max_lev){
    table(factor(vec, levels = 0:max_lev)) %>%
        as.data.frame() %>%
        rename(RawScore=Var1) %>%
        mutate(CumFreq=cumsum(Freq),
               CumPctile=floor(100*CumFreq/max(CumFreq)))
}
