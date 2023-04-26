#' DIF_poly_shw
#'
#' This function extracts item statistics from subgroups of the polytmous
#' DIF variable (in 'test_category.shw' files in DIF variable's subfolder
#' within 'DIF' folder) to produce a list of dataframes of those statistics.
#' Then it calls function DIF_poly() to perform a Bonferroni-adjusted
#' statistical test and flag the combinations of item and category that show
#' significant results.
#'
#' @param DIFVar Name of polytomous DIF variable, e.g., 'nation'.
#' @param labels Dataframe of labels of all the items in a test. Variable
#' 'item' is item order in label file ('test_Labels.txt' in 'data' folder)
#' and in test. Variable 'label' is complete item labels of test items. Default
#' is NULL.
#' @param test Name of the test.
#' @param domain Name of the domain in the test, e.g., 'Literacy'. Default is NULL.
#' @param p_cut Threshold of statistical test. Default is 0.05.
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @return List of summary of results from polytomous DIF variable analysis,
#' including comments, step, summary statistics with flags, and statistics used
#' to produce the the flags.
#' @return Tibble of summary of results from polytomous DIF variable analysis.
#' @examples
#' DIF_poly_shw(DIFVar='quintile', test='ELNA')
#' @export

DIF_poly_shw <- function(DIFVar, test, domain=NULL, p_cut=0.05, step=FALSE, labels=NULL){
  if (is.null(labels)){
    labels <- read.table(paste0('data/', test, '_Labels.txt')) |>
      rownames_to_column() |>
      `colnames<-`(c('item', 'label')) |>
      mutate(item=as.integer(item))
  }

  folder <- paste0('DIF/', DIFVar)
  file_ls <- list.files(folder, full.names=TRUE) |>
    str_subset(test) |>
    str_subset('_shw.xls')
  file_ls <- file_ls[!grepl("facet", file_ls)]
  file_ls <- file_ls[!grepl("group", file_ls)]

  cats <- str_sub(file_ls, -10, -8) |>
    parse_number() #categories

  # order categories
  ord <- cats |> order()
  file_ls <- file_ls[ord]
  cats <- cats[ord]

  # delta
  tests <- paste0(test, '_', cats)
  getDf <- function(i){
    readxl::read_xls(
        paste0(folder, '/', tests[[i]], '_shw.xls'),
        sheet='ResponseModel',
        skip=5,
        n_max=N_item(folder, tests[[i]])+1,
        .name_repair = "unique_quiet"
      ) |>
      select(
        item,
        delta=ESTIMATE,
        error=`ERROR^`
      ) |>
      dplyr::filter(!is.na(item))
  }
  df_ls <- map(seq_along(cats), getDf)

  DIF_poly(df_ls=df_ls, DIFVar=DIFVar, cats=cats, labels=labels,
        test=test, domain=domain, p_cut=p_cut, step=step)
}
