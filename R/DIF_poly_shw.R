#' DIF_poly_shw
#'
#' This function extracts item statistics from subgroups of the polytmous DIF variable (in 'test_category.shw' files in DIF variable's subfolder within 'DIF' folder) to produce a list of dataframes of those statistics. Then it calls function DIF_poly() to perform a Bonferroni-adjusted statistical test and flag the combinations of item and category that show significant results.
#'
#' @param folder Folder where subgroups' calibration results are located. Default is the subfolder with name of argument 'DIFVar' within 'DIF' folder (folder=NULL). It's advisable to put factor level in file names such as 'test_4.shw' and put all groups' .shw files in this folder.
#' @param DIFVar Name of polytomous DIF variable, e.g., 'nation'.
#' @param labels Dataframe of labels of all the items in a test. Variable 'item' is item order in label file ('test_Labels.txt' in 'data' folder) and in test. Variable 'label' is complete item labels of test items.
#' @param test Name of the test.
#' @param domain Name of the domain in the test, e.g., 'Literacy'. Default is NULL.
#' @param p_cut Threshold of statistical test. Default is 0.05.
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @return List of summary of results from polytomous DIF variable analysis, including comments, step, summary statistics with flags, and statistics used to produce the the flags.
#' @return Tibble of summary of results from polytomous DIF variable analysis.
#' @examples
#' DIF_poly_shw(DIFVar='quintile', labels=labels, test=test)
#' @export

DIF_poly_shw <- function(folder=NULL, DIFVar, labels, test, domain=NULL,
                         p_cut=0.05, step=FALSE){
    if (is.null(folder)) folder <- here::here('DIF', DIFVar)
    file_ls <- list.files(folder, full.names=TRUE) %>%
        str_subset(test) %>%
        str_subset('.shw') %>%
        .[!grepl("facet", .)] %>%
        .[!grepl("group", .)]

    cats <- str_sub(file_ls, -6, -5) %>%
        parse_number() #categories

    # order categories
    ord <- cats %>% order()
    file_ls <- file_ls[ord]
    cats <- cats[ord]

    N <- map_dbl(cats, ~N_item(folder=folder, test=paste0(test, '_', .)))

    # delta
    Term1_lines <- map_dbl(file_ls, ~readLines(.) %>%
                               str_detect('TERM 1: item') %>%
                               which() %>% .[2])
    dif_stats <- pmap(list(a=file_ls, b=Term1_lines, c=N),
                      function(a,b,c) {
                          read_fwf(a, fwf_cols(
                              item=c(1, 5),
                              delta=c(21, 28),
                              error=c(31, 35)), ##item=c(6, 20),
                              skip=(b + 5), n_max=c,
                              show_col_types = FALSE) %>%
                              mutate(item = as.character(item),
                                     delta = parse_number(delta))
                      })

    DIF_poly(df_ls=dif_stats, DIFVar=DIFVar, cats=cats, labels=labels,
                test=test, domain=domain, p_cut=p_cut, step=step)
}
