#' CCC_fine_flag4
#'
#' This function adds comments by comparing flags of priorities 1 to 3 and comments only based on CCC. If item statistics show issues but CCC are fine, comments indicate so. Also, if CCC shows issues but item statistics are fine, comments indicated so.
#'
#' @param iflag123 Item statistics with flag priorities of 1 to 3.
#' @param comments Dataframe with Comments.
#' @return Item statistics with four levels of priority.
#' @export

CCC_fine_flag4 <- function(iflag123, comments){
  for (i in 1:nrow(iflag123)) {
     iNumInCom <- which(comments$iNum==i)
     if (identical(iNumInCom, integer(0))){
       next
     }
     if (!(iflag123[i, ]$Priority %in% c(NA, '', ' '))){
       if (comments[iNumInCom, ]$Comment %in% c(NA, '', ' ')){
         if (comments[iNumInCom, ]$itype %in% c('dich', 'score')){
           comments[iNumInCom, ]$Comment <- 'CCC indicates item functioned appropriately at high ability levels.'
         }
         if (comments[iNumInCom, ]$itype=='poly'){
           comments[iNumInCom, ]$Comment <- 'Categories separated well.'
         }
       }
     }
     if (!(comments[iNumInCom, ]$Comment %in% c(NA, '', ' '))){
       if (iflag123[i, ]$Priority %in% c(NA, '', ' ')){
         iflag123[i, ]$Priority <- 4
       }
     }
  }

  iflag123 |>
    left_join(
      comments |> dplyr::select(seqNo=iNum, Comment),
      by='seqNo'
    )
}
