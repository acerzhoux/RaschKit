#' opt_stats
#'
#' This function extracts option statistics from 'test.txt' file.
#' This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @return Dataframe of option statistics.
#' @examples
#' b <- opt_stats(test='math_35')
#' @export

opt_stats <- function(test){
  options(warn=-1)

  n_item <- N_item2('output', test)
  opt_lines <- Lines('output', test, 'opt', '===============')
  n_opt <- (opt_lines[3] - opt_lines[2] - 1)
  n_cat <- n_opt / n_item

  readxl::read_xls(
      paste0('output/', test, '_opt.xls'),
      skip=4,
      n_max=n_opt,
      # col_types='numeric',
      .name_repair = "unique_quiet"
    ) |>
    `names<-`(c('resp', 'iScore', 'count', '%correct',
          'ptBis', 't', 'p', 'pv1Avg', 'pvSD')) |>
    mutate(
      seqNo = rep(1:n_item, times=1, each=n_cat)
    )
}

