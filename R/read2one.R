#' read2one
#'
#' This function reads from 'results' folder Excel files associated with 'tests'
#' and puts them into one file.
#'
#' @param subfolder Folder where to-be-read files are located. Must be one of
#' c('results', 'DIF', 'equating').
#' @param tests Vector of test names after '_'.
#' @param prefix Common prefix in file names before first '_'. Default is NULL.
#' @param file_name Name given to new file, each sheet of which is one test's result.
#' Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Default is NULL.
#' Should be specified when to read multiple DIF analysis (binary DIF variable)
#' Excels and plots from 'DIF' folder.
#' Its order corresponds to the alphabetic/numeric order of DIF variables'
#' two categories in data.
#' @examples
#' # Not run
#' # read2one(subfolder='equating', tests=c('AHU','MST','R','N'))
#' @export

read2one <- function (subfolder = c('results', 'DIF', 'equating'), tests, prefix=NULL,
                      file_name = NULL, vars=NULL){
    folder <- here::here(subfolder)

    # Excel names to read files from
    if (subfolder == 'equating'){
        file_ls <- map(tests, ~list.files(folder, pattern=.x, full.names=TRUE))
        files <- file_ls %>%
            map(~str_subset(.x, '.xlsx')) %>%
            unlist()
        in_dif <- file_ls %>%
            map(~str_subset(.x, '.pdf')) %>%
            unlist()
    } else {
        files <- file.path(folder, str_c(prefix, '_', tests, '.xlsx'))
    }

    # sheet name to read from
    sheetNm <- ifelse(subfolder %in% c('DIF', 'equating'), 'flag', 1)
    ex_ls <- map(files, ~readxl::read_xlsx(.x, sheetNm))

    # Excel name to save files into one
    if (is.null(file_name)) {
        if (subfolder == 'equating'){
            file_name <- list.files(folder, pattern=tests[[1]]) %>%
                str_subset('.pdf') %>%
                strsplit('_') %>%
                unlist() %>%
                .[length(.)] %>%
                str_sub(1, -5)
            file <- file.path(folder, str_c(file_name, ".xlsx"))
        } else {
            file <- file.path(folder, str_c(prefix, ".xlsx"))
        }
    } else {
        file <- file.path(folder, str_c(prefix, '_', file_name, '.xlsx'))
    }

    # save results
    if (sheetNm == 'flag'){
        # combine excels
        names(ex_ls) <- tests
        if (subfolder == 'equating'){
            summary <- map(files, ~readxl::read_xlsx(.x, 'shift'))  %>%
                map2(tests, ~mutate(.x, Domain=.y)) %>%
                map2(map(files, ~readxl::read_xlsx(.x, 'flag')),
                     ~mutate(.x, Links_bfr=nrow(.y),
                             Links_afr=nrow(filter(.y, is.na(flag))),
                             Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%'))) %>%
                reduce(bind_rows) %>%
                select(Domain, everything())
            sum_ls <- list(Shift=summary) %>%
                append(ex_ls)
        } else {
            summary <- map(ex_ls, ~.x %>% filter(flag==1)) %>%
                imap(~.x %>% mutate(Domain=.y)) %>%
                map(~.x %>% mutate(favor=as.character(ifelse(DIF<0, vars[[1]], vars[[2]])))) %>%
                reduce(bind_rows) %>%
                select(Domain, favor, everything()) %>%
                arrange(Domain, favor, desc(chisq))
            sum_ls <- list(Summary=summary) %>%
                append(ex_ls)
        }
        sum_ls %>%
            writexl::write_xlsx(file)

        # combine pdf's
        if (subfolder == 'DIF') {
            in_dif <- file.path(folder, str_c(prefix, '_', tests, '.pdf'))
            out_dif <- file.path(folder, str_c(prefix, '.pdf'))
            qpdf::pdf_combine(input = in_dif, output = out_dif)

            in_facil <- file.path(folder, str_c(prefix, '_', tests, '_Facility.pdf'))
            out_facil <- file.path(folder, str_c(prefix, '_Facility.pdf'))
            qpdf::pdf_combine(input = in_facil, output = out_facil)
        } else {
            out_dif <- file.path(folder, str_c(file_name, ".pdf"))
            qpdf::pdf_combine(input = in_dif, output = out_dif)
        }

        writeLines(c(
            paste0('Files and plots combined to:'),
            paste0('\tDIF flags:\t', file),
            paste0('\tDIF plots:\t', out_dif),
            if (subfolder == 'DIF') paste0('\tFacility plots:\t', out_facil)))
    }
    if (sheetNm == 1){
        ex_ls %>%
            `names<-`(tests) %>%
            writexl::write_xlsx(file)

        writeLines(paste0('Files combined to: ', file))
    }
}