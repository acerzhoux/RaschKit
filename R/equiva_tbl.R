#' equiva_tbl
#'
#' This function extracts desired type of ability estimates from compressed ConQuest .cqs file and generate scaled scores with specified slope and intercept. This is associated with test named 'test'.
#'
#' @param wd Working directory. Default is the folder where .Rproj is located.
#' @param test Name of test.
#' @param est_type Type of ability estimate to use, including 'wle', 'mle', 'pv1' 'pv2', 'pv3', 'pv4', and 'pv5'. Default is 'wle'.
#' @param slope Slope to multiply ability estimates. Default is 10.
#' @param intercept Value/intercept to add to ability estimates. Default is 100.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates. Default is TRUE.
#' @return Dataframe of scaled ability estimates.
#' @examples
#' equiva_tbl()
#' @export

equiva_tbl <- function(wd=here::here(), test, est_type='wle', slope=10,
                       intercept=100, extrapolation=TRUE){
    n_item <- N_item(folder=here::here('Output'), test=test)
    equiva_path <- here::here('Output', paste0(test, '.eqv'))
    cn <- c('reset;',
            str_c('get << ', here::here('Output', paste0(test, '_compressed.CQS')), ';', sep=''),
            str_c('equivalence ', est_type, ' >> ', equiva_path, ';', sep=''),
            'reset all;',
            'quit;'
    )
    cqc_path <-  file.path(wd, 'Input', paste0(test, '_eqv.cqc'))
    writeLines(cn, cqc_path)

    # call CQ and generate equiv table
    conquestr::ConQuestCall(
        file.path('C:', 'Program Files', 'ACER ConQuest', 'ConQuestConsole.exe'),
        cqc=cqc_path
    )

    data_read <- read_fwf(equiva_path,
                          fwf_cols(Score_raw=c(2, 10), EST=c(13, 20), SE=c(24, 30)),
                          skip=10, n_max=(n_item +1)) %>%
        modify_at(2:3, ~as.numeric(.) %>% round(., digits=3)) %>%
        modify_at(1, ~as.numeric(.) %>% round(., digits=0))

    # extrapolation
    if (extrapolation){
        est_min <- data_read[2:4,]$EST %>%
            {.[[3]] - .[[2]]*3 + .[[1]]*3}
        est_max <- data_read[(n_item-2):n_item,]$EST %>%
            {.[[3]]*3 - .[[2]]*3 + .[[1]]}

        data_read[1, ]$EST <- est_min
        data_read[(n_item+1), ]$EST <- est_max
    }

    data_read %>%
        mutate(Score_scale=round(slope*EST + intercept, digits=0))
}
