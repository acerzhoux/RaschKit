#' equiva_tbl
#'
#' This function extracts desired type of ability estimates from compressed
#' ConQuest .cqs file and generate scaled scores with specified slope and
#' intercept. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @param est_type Type of ability estimate to use for score equivalence table,
#' 'wle' or 'mle'. Default is 'wle'.
#' @param slope Slope to multiply ability estimates. Default is NULL
#' @param intercept Value/intercept to add to ability estimates. Default is NULL.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates.
#' Default is FALSE.
#' @return Dataframe of scaled ability estimates.
#' @examples
#' equiva_tbl(test='Writing')
#' @export

equiva_tbl <- function(test, est_type='wle', slope=NULL,
                       intercept=NULL, extrapolation=FALSE){
    equiva_path <- paste0('output/', test, '_eqv.txt')
    cn <- c(
        'reset;',
        paste0('get << ', 'output/', test, '_compressed.CQS;'),
        paste0('equivalence ', est_type, ' >> ', equiva_path, ';'),
        'reset all;',
        'quit;'
    )
    cqc_path <-  paste0('input/', test, '_eqv.cqc')
    writeLines(cn, cqc_path)

    # call CQ and generate equiv table
    conquestr::ConQuestCall(
        cqc=cqc_path,
        stdout=NULL
    )

    # read equiv tbl
    lines <- readLines(equiva_path)
    ind2 <- grep('=====', lines)[[2]]-1
    eqv_tbl <- str_replace_all(lines[11:ind2], '_BIG_', ' NA') %>%
        read.table(text=.) %>%
        as_tibble() %>%
        `colnames<-`(c('Score_raw','EST','SE'))

    # extrapolation
    if (extrapolation){
        tbl_r <- nrow(eqv_tbl)-1
        est_min <- eqv_tbl[2:4,]$EST %>%
            {.[[3]] - .[[2]]*3 + .[[1]]*3}
        est_max <- eqv_tbl[(tbl_r-2):tbl_r,]$EST %>%
            {.[[3]]*3 - .[[2]]*3 + .[[1]]}

        eqv_tbl[1, ]$EST <- est_min
        eqv_tbl[(tbl_r+1), ]$EST <- est_max
    }
    writexl::write_xlsx(eqv_tbl, paste0('results/', 'eqv_', test, '.xlsx'))

    # scale scores
    if (!is.null(intercept) | !is.null(slope)){
        cat('Generating scaled-score table...\n')
        if (is.null(intercept)) intercept <- 0
        if (is.null(slope)) slope <- 1
        scaled_tbl <- eqv_tbl %>%
            mutate(Score_scale=round(slope*EST + intercept, digits=0))
        # save results and return
        writexl::write_xlsx(scaled_tbl,
                            paste0('results/', 'scaled_', test, '.xlsx'))
    }
}
