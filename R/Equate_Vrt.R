#' Equate_Vrt
#'
#' This function performs vertical equating. An Excel file named 'Vrt_xxx.xlsx'
#' is saved in 'equating' folder, each sheet of which is for two
#' adjacent grades. Also saved are scatterplots named 'Vrt_xxx.pdf'.
#'
#' @param test Name of test.
#' @param grades Vector of all grades. Default is c(2:10).
#' @param grade_name Character to add before grade in plots. Default is 'L'
#' (for 'level').
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE.
#' @return List of chi-square test results for anchors between two adjacent grades.
#' @examples
#' Equate_Vrt(test='bang', grades=c(3, 5))
#' @export

Equate_Vrt <- function(test, grades=c(2:10), grade_name='L', p_cut=0.05,
                       DIF_cut=0.5, DIF_adj_cut=4, step=FALSE, iterative=FALSE){
  if (!dir.exists('equating')) dir.create('equating')

  n_test <- length(grades)
  equat_ls <- list()
  for (i in 1:(n_test-1)){
    test_2 <- c(grades[[i]], grades[[i+1]])
    cat('Equating Grades',test_2,'...\n')

    # extract facility and discrimination from its.xls file
    t1 <- paste0(test, '_', test_2[[1]])
    t2 <- paste0(test, '_', test_2[[2]])
    facilDiscrFitw <- df_its('output', t1) |>
      inner_join(
        df_its('output', t2),
        by = 'Label'
      ) |>
      dplyr::select(-contains('iNum.')) |>
      na.omit()

    names(facilDiscrFitw) <- gsub("\\.x", paste0('_L', test_2[[1]]), names(facilDiscrFitw))
    names(facilDiscrFitw) <- gsub("\\.y", paste0('_L', test_2[[2]]), names(facilDiscrFitw))

    pstats <- plot_facilDiscrFitw(facilDiscrFitw, paste0('L', test_2), c(3, 7), 3)

    statsEqu <- Equate_shw(test, test_2, grade_name, p_cut, DIF_cut,
                           DIF_adj_cut, FALSE, step, iterative)

    statsEqu[['flag']] <- left_join(
      statsEqu[['flag']],
      facilDiscrFitw,
      by = c('item' = 'Label')
    ) |>
    dplyr::select(item, contains('N'), contains('Facil'),
       contains('Discr'), contains('Fitw'), everything())

    statsEqu[['final']] <- left_join(
      statsEqu[['final']],
      facilDiscrFitw,
      by = c('item' = 'Label')
    ) |>
    dplyr::select(
      item, contains('N'), contains('Facil'),
      contains('Discr'), contains('Fitw'),
      everything()
     )

    statsEqu[['plot_DIF']] <- list(statsEqu[['plot_DIF']], pstats)

    equat_ls[[paste0(test_2, collapse='_')]] <- statsEqu
  }

  summary <- map(equat_ls, 'shift') |>
    imap(~mutate(.x, Grade=.y)) |>
    map2(map(equat_ls, 'final'),
      ~mutate(
      .x,
      Links_bfr=nrow(.y),
      Links_afr=nrow(filter(.y, flag==0)),
      Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%')
      )
    ) |>
    reduce(bind_rows) |>
    select(Grade, everything())
  sum_ls <- list(Summary=summary) |>
    append(map(equat_ls, 'final'))

  # save results
  path_xlsx <- paste0('equating/', 'Vrt_', test, if(step) '_step', '.xlsx')
  writexl::write_xlsx(sum_ls, path_xlsx)

  path_pdf <- paste0('equating/', 'Vrt_', test, if(step) '_step', '.pdf')
  pdf(path_pdf, width=7, height=14)
  map(map(equat_ls, 'plot_DIF'), print)
  dev.off()

  # point users to files of varying purposes
  writeLines(c(
    paste0('Anchor DIF analysis for ', test, ' (before vertical equating):'),
    paste0('\tSummary:\t', path_xlsx),
    paste0('\tPlots:\t\t', path_pdf)
  ))
}
