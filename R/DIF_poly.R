#' DIF_poly
#'
#' This function performs DIF analysis on a polytomous variable and summarizes results.
#'
#' This functions saves a multiple-facet plot of plots for each subgroup's
#' average delta (x) vs. subgroup delta (y) in the working directory's 'DIF'
#' folder with the DIF variable in the name. Also saved in the 'DIF' folder is
#' an Excel file with four sheets, 'comments', 'step', 'flags', and 'stats'.
#'
#' @param df_ls List of dataframes of test calibration results for various
#' groups of the poloymous DIF variable. Each list element of the dataframe
#' includes variables of item (item order in a single calibration of one
#' DIF variable group), delta, and error.
#' @param DIFVar Name of polytomous DIF variable.
#' @param cats Vector of categories of the polytomous DIF variable,
#' e.g., c(4, 5, 6, 7).
#' @param labels Dataframe of labels of all the items in a test with two
#' variable of 'item' and 'label'. 'Item' is the item order in the test.
#' 'Label' is the item label.
#' @param test Name of the test.
#' @param domain Name of the domain in the test, e.g., 'Literacy'.
#' Default is NULL.
#' @param p_cut Threshold of statistical test. Default is 0.05.
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @return List of summary of results from polytomous DIF variable analysis,
#' including comments, step, summary statistics with flags, and statistics
#' used to produce the the flags.
#' @examples
#' DIF_poly()
#' @export

DIF_poly <- function(df_ls, DIFVar, cats, labels, test, domain=NULL,
           p_cut=0.05, step=FALSE){
  delta_cat_item <- map(df_ls, ~dplyr::select(.x, item, delta)) |>
    reduce(function(x, y) inner_join(x, y, by='item')) |>
    `colnames<-`(c('item', str_c('delta_', cats))) |>
    mutate(item=as.character(item)) |>
    inner_join(labels, by='item') |>
    dplyr::select(-item) |>
    dplyr::rename(item=label)

  items <- delta_cat_item$item
  delta_cat <- delta_cat_item |> dplyr::select(-item)
  delta_cat_dif <- round(delta_cat[] - rowMeans(delta_cat[]), 3)

  # error
  error_cat_item <- map(df_ls, ~dplyr::select(.x, item, error)) |>
    reduce(function(x, y) inner_join(x, y, by='item')) |>
    mutate(item=as.character(item)) |>
    inner_join(labels, by='item') |>
    dplyr::select(-item) |>
    dplyr::rename(item=label) |>
    `colnames<-`(c(str_c('Error_',cats), 'item'))
  error_cat <- error_cat_item |> dplyr::select(-item)

  # #### Bonferroni adjusted normal distribution
  n_cat <- length(df_ls)
  t <- (delta_cat_dif / (error_cat*2)) |> # standardized diff
    `colnames<-`(c(str_c('std_diff_', cats)))
  n_comp <- n_cat*(n_cat-1)/2
  threshold <- qnorm(1 - p_cut/n_comp/2, 0, 1) # Excel: NORMINV(p, m, s)
  Flag_dif_all <- map2(t, delta_cat_dif, ~DIF_symbol_poly(threshold, .x, .y))
  Flag_dif <- Flag_dif_all |>
    map(~dplyr::select(., '...1', '...2')) |>
    map2(cats, ~`colnames<-`(.x, c(paste0('Delta_adj_', .y), paste0('sig_', .y)))) |>
    reduce(bind_cols)
  delta_cat$delta_Ave <- round(rowMeans(delta_cat[]), 3)

  # plots
  plist <- map(
      1:length(cats),
      ~plot_DIF_poly(
        DIFVar, ., cats, error_cat_item, delta_cat, Flag_dif, error_cat
      )
    )
  p_save <- patchwork::wrap_plots(plist, ncol=floor(sqrt(length(plist)))) +
    plot_annotation(title=paste0(toupper(test),
      if (!is.null(domain)) paste0(' ', domain),
      ': ', toupper(DIFVar), ' DIF Review on ', nrow(delta_cat), ' items')) +
    labs(x=paste0('Average Difficulty (Logits)'),
       caption=paste0('DIF: Bonferroni Adj. Sig. Test'))

  # summary of results
  DiF_sum <- cbind(
    items,
    {
      Flag_dif_all |>
      map(~dplyr::select(., '...3')) |>
      map2(cats, ~`colnames<-`(.x, .y)) |>
      reduce(bind_cols)
    }
  )
  comments <- DIF_comment_poly(DiF_sum = DiF_sum)

  # save results and plots
  folder <- paste0('DIF/', DIFVar)
  prefix <- ifelse(
      step,
      paste0(folder, '/step_', test, if (!is.null(domain)) paste0('_', domain), '_'),
      paste0(folder, '/', test, if (!is.null(domain)) paste0('_', domain),  '_')
  )
  ggsave(
      paste0(prefix, 'delta.png'),
      p_save,
      width=34, height=60, units="cm"
  )

  names(Flag_dif) <- gsub("_", ' ', names(Flag_dif))

  results <- list(
    comments = comments,
    steps = DIF_steps_poly(),
    final=cbind(items, Flag_dif),
    stats=cbind(items, delta_cat, error_cat, round(t, 3))
  )
  writexl::write_xlsx(results, paste0(prefix, 'process.xlsx'))

  # generate report
  # output <- results |> append(list(plot_DIF=p_save))

  # rmd_file <- system.file("rmd", "DIF_poly.Rmd", package = "RaschKit")
  # rmarkdown::render(
  #   rmd_file,
  #   params = list(output=output, test=test, DIFVar=DIFVar),
  #   output_file = str_c(DIFVar, '_', test, '.html'),
  #   output_dir = 'DIF',
  #   quiet = TRUE
  # )

  # point users to files of varying purposes
  # writeLines(c(
  #   paste0('\n========= Output Files =========\n'),
  #   paste0(toupper(DIFVar), ' DIF analysis for ', test,
  #        if (step) ' (step)', ' (Bonferroni adjusted tests):'),
  #   paste0('\tSummary:\t', 'DIF/', sht, '.xlsx'),
  #   paste0('\tDIF Plot:\t', 'DIF/', sht, '.pdf'),
  #   if (file.exists(rmd_file)){
  #     paste0('\tDIF report:\t', 'DIF/', DIFVar, '_', test, '.html')
  #   }
  # ))

  # output
}
