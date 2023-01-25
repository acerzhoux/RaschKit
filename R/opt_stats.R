#' opt_stats
#'
#' This function extracts option statistics from 'test.txt' file.
#' This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @return Dataframe of option statistics.
#' @examples
#' opt_stats(test='FPA')
#' @export

opt_stats <- function(test){
    n_item <- N_item2('output', test)
    opt_lines <- Lines('output', test, 'opt', '===============')
    n_opt <- (opt_lines[3] - opt_lines[2] - 1)
    n_cat <- n_opt / n_item
    qids <- as.character(qid_its('output', test))

    read_fwf(
        Path('output', test, 'opt'),
        fwf_cols(
            resp = c(1, 6),
            iScore = c(11, 17),
            count = c(18, 29),
            `%correct` = c(30, 37),
            ptBis = c(38, 47),
            t = c(48, 52),
            p = c(54, 57),
            pv1Avg = c(60, 65)
        ),
        skip = opt_lines[2],
        n_max = n_opt,
        na = c('', 'NA'),
        show_col_types = FALSE
    ) |>
    mutate(
        qOrder = rep(qids, times=1, each=n_cat)
    )
}

