#' rater_agreement_stats
#'
#' This function calculates types of rater agreement statistics, including 
#' agreements, Fleiss, ICC, and Pearson correlation.
#'
#' @param df Dataframe with scores on scoring criteria. Each dimension has 
#' at least two scores. Each criterion should have unique char string in the 
#' variable names.
#' @param dimensions Unique char string in the variable names associated with 
#' one scoring criterion.
#' @return Dataframe with types of rater agreement statistics.
#' @examples
#' rater_agreement_stats()
#' @export

rater_agreement_stats <- function(df, dimensions){
    # agreements
    tols <- c(0,1)
    tl_vec <- NULL
    for (tl in tols){
        agreement <- NULL
        for (dm in dimensions){
            a <- df %>%
                select(contains(dm)) %>%
                agree(tolerance=tl) %>%
                pluck('value')/100
            agreement <- c(agreement,a)
        }
        tl_vec <- rbind(tl_vec,agreement)
    }
    # Fleiss' Kappa
    Fleiss <- NULL
    for (dm in dimensions){
        a <- df %>%
            select(contains(dm)) %>%
            `kappam.fleiss` %>%
            pluck('value')
        Fleiss <- c(Fleiss,a)
    }
    # ICC
    `ICC` <- NULL
    for (dm in dimensions){
        a <- df %>%
            select(contains(dm)) %>%
            icc("twoway", "agreement") %>%
            pluck('value')
        `ICC`  <- c(`ICC` ,a)
    }
    # Pearson's R
    Pearson <- NULL
    for (dm in dimensions){
        a <- df %>%
            select(contains(dm)) %>%
            meancor %>%
            pluck('value')
        Pearson <- c(Pearson,a)
    }
    agr <- rbind(tl_vec, Fleiss, `ICC`, Pearson)

    return(agr)
}
