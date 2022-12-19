#' opt_stats
#'
#' This function extracts option statistics from 'test.txt' file. 
#' This is associated with test named 'test'.
#'
#' @param folder Folder where 'test.txt' file is located.
#' @param test Name of test.
#' @return Dataframe of option statistics.
#' @examples
#' opt_cq <- opt_stats(test='elana_poly_score')
#' @export

opt_stats <- function(folder=here::here('output'), test){
    n_item <- N_item2(folder=folder, test=test)
    opt_path <- list.files(folder, full.names=TRUE) %>%
        str_subset(paste0(test, '.txt'))
    opts_str <- opt_path %>%
        readLines()
    opt_lines <- opts_str %>%
        str_detect('===============') %>% which()
    n_opt <- (opt_lines[3] - opt_lines[2] - 1)
    n_cat <- n_opt / n_item
    qids <- qid_its(folder=folder, test=test) %>%
        as.character()

    read_fwf(opt_path, fwf_cols(resp=c(1, 6),
                      iScore=c(11, 17),
                      count=c(18, 29),
                      `%correct`=c(30, 37),
                      ptBis=c(38, 47),
                      t=c(48, 52),
                      p=c(54, 57),
                      pv1Avg=c(60, 65)),
             skip=opt_lines[2],
             n_max=n_opt,
             na=c('', 'NA'),
             show_col_types = FALSE
        ) %>%
        mutate(qOrder=rep(qids, times=1, each=n_cat))
}

