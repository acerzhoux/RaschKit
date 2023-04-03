#' DIF_domain
#'
#' This function performs DIF analysis on variables and responses in the data.
#' Data should be processed to have DIF variables of only categories needed.
#' This is associated with test named 'test'. An Excel file with a summary of
#' the DIF analysis results and plots will be saved in 'DIF' folder in the
#' working directory.
#'
#' @param test Name of the test.
#' @param data Dataframe of DIF variables and responses.
#' @param var_ls List of DIF variables. Name is variable name. Element is name
#' of categories which corresponds to data coding order.
#' @param pid Name of candidates' ID variable.
#' @param n_cov Number of covariates before responses.
#' @param keys Vector of keys in the test.
#' @param labels Vector of item labels that correspond to order of item response
#' columns in data.
#' @param poly_key TRUE if the key of any item has polytomous scoring.
#' Default is FALSE.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param facil_cut Threshold of number of percent to flag an item with large
#' facility difference between two groups of test takers. Default is 10.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. Default
#' is FALSE.
#' @param miss_code Missing codes. Default is c('M', 'R').
#' @param output_ready TRUE if results are ready. Default is FALSE.
#' @param iterative TRUE to iteratively remove DIF items. Default is TRUE.
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @examples
#' DIF_domain()
#' @export

DIF_domain <- function(test, data=NULL,
             var_ls=NULL, n_cov=NULL, pid=NULL,
             keys=NULL, labels=NULL, poly_key=FALSE,
             p_cut=0.05, DIF_cut=0.5,
             DIF_adj_cut=4, facil_cut=10,
             desig_effect=1, step=FALSE, output_ready=FALSE,
             miss_code=c('M', 'R'), iterative=TRUE, quick=TRUE){
  if (test %in% c('Writing','writing','W','w','WA','wa','WB','wb')){
    poly_key <- TRUE
  }

  n_var <- length(var_ls)
  vars_DIF <- names(var_ls)
  ls <- list()
  for (i in 1:n_var){
    DIFVar <- vars_DIF[[i]]
    var_D <- var_ls[[i]]
    if (output_ready){
      ls[[i]] <- DIF_dich_its_shw(DIFVar=DIFVar, test=test,
        vars=var_D, p_cut=p_cut, DIF_cut=DIF_cut,
        DIF_adj_cut=DIF_adj_cut,
        desig_effect=desig_effect, step=step, facil_cut=facil_cut,
        long_label=FALSE, save_xlsx=FALSE,
        iterative=iterative, quick=quick)
    } else {
      ls[[i]] <- DIF_dim_one(method='chi_square', test=test,
         pid=pid, n_cov=n_cov,
         DIFVar=DIFVar, data=data, keys=keys, vars=var_D,
         labels=labels, miss_code=miss_code,
         save_xlsx=FALSE, poly_key=poly_key,
         p_cut=p_cut, DIF_cut=DIF_cut,
         DIF_adj_cut=DIF_adj_cut, facil_cut=facil_cut,
         desig_effect=desig_effect, iterative=iterative,
         step=step, quick=quick)
    }
  }

  # save to Excel file
  ex_ls <- map(ls, 'final')
  names(ex_ls) <- vars_DIF
  summary <- map(ex_ls, ~.x |> filter(flag==1)) |>
    imap(~.x |> mutate(variable=.y)) |>
    map2(
      var_ls,
      ~.x |> mutate(favor=as.character(ifelse(DIF<0, .y[[1]], .y[[2]])))
    ) |>
    reduce(bind_rows) |>
    select(variable, favor, everything()) |>
    arrange(variable, favor, item)
  sum_ls <- list(summary=summary) |>
    append(ex_ls)
  writexl::write_xlsx(sum_ls, here::here('DIF', paste0(test, if (step) '_step', '.xlsx')))

  # ####### generate Difficulty scatterplot
  pdf(file=file.path(here::here('DIF'), paste0(test, if (step) '_step', "_Difficulty.pdf")),
    width=7, height=10)
  map(map(ls, 'plot_DIF'), ~print(.x))
  dev.off()

  if (!step){
    # ####### generate Facility scatterplot
    pdf(file=file.path(here::here('DIF'), paste0(test, "_Facility.pdf")),
      width = 7, height = 7)
    map(map(ls, 'plot_facil'), ~print(.x))
    dev.off()
  }
}
