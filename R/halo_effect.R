#' halo_effect
#'
#' This function calculates halo effect indexes for marks on constructed responses.
#'
#' @param df Dataframe with only double marks on scoring criteria. The first half of columns are the first marks on each criterion, and the second half the second marks.
#' @param no_dim Number of scoring criteria.
#' @return Dataframe of indices to flag halo effect. The default threshold is 1.2.
#' @examples
#' halo_effect()
#' @export

halo_effect <- function(df, no_dim){
    rater_1 <- df[1:no_dim]
    rater_2 <- df[(no_dim+1):(no_dim*2)]

    i_vec <- NULL
    for (i in 1:no_dim){
        j_vec <- NULL
        for (j in 1:no_dim){
            a <- sd(rater_1[[i]]-rater_2[[j]])/sd(rater_1[[i]]-rater_1[[j]])
            j_vec <- c(j_vec,a)

        }
        i_vec <- rbind(i_vec,j_vec)
    }

    i_vec[i_vec==Inf] <- NA
    i_vec <- round(i_vec, 2)

    return(i_vec)
}
