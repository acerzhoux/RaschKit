#' priority_3
#'
#' This function checks response option point biserial (Pt Bis) values in 
#' 'test.itn' file in 'output' folder and flag items with any distractor of 
#' positive Pt Bis. This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 
#' 'test'. Default is 'output' folder in working directory.
#' @param test Name of test.
#' @return Vector of flags.
#' @examples
#' priority_3(test='FPA')
#' @export

priority_3 <- function(folder=here::here('output'), test){
    priority3=itn_ls(folder=folder, test=test) %>%
        map(~filter(., Score == 0)) %>%
        map_lgl(~any(.$PtBis > 0))
    tibble(priority_3=priority3,
        qOrder=1:length(priority3))
}
