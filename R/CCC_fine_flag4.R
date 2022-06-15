#' CCC_fine_flag4
#'
#' This function adds comments by comparing flags of priorities 1 to 3 and comments only based on CCC. If item statistics show issues but CCC are fine, comments indicate so. Also, if CCC shows issues but item statistics are fine, comments indicated so.
#'
#' @param iflag123 Item statistics with flag priorities of 1 to 3.
#' @param comments Dataframe with Comments.
#' @return Item statistics with four levels of priority.
#' @examples
#' CCC_fine_flag4()

CCC_fine_flag4 <- function(iflag123, comments){
  qorders <- unique(comments$iNum)
  for (i in qorders) {
     if (!(iflag123$Priority[[i]] %in% c(NA, '', ' '))){
       if (comments$Comment[[i]] %in% c(NA, '', ' ')){
         if (comments[i, ]$itype %in% c('dich', 'score')){
           comments$Comment[[i]] <- 'CCC indicates item functioned appropriately at high ability levels.'
         }
         if (comments[i, ]$itype=='poly'){
           comments$Comment[[i]] <- 'Categories separated well.'
         }
       }
     }
     if (!(comments$Comment[[i]] %in% c(NA, '', ' '))){
       if (iflag123$Priority[[i]] %in% c(NA, '', ' ')){
         iflag123$Priority[[i]] <- '4'
       }
     }
  }
  iflag123$Comments <- comments$Comment
  iflag123
}
