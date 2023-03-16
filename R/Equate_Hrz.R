#' Equate_Hrz
#'
#' This function performs horizontal equating. An Excel file with
#' 'Equating_Horizontal' in name is saved in 'equating' folder, each sheet of
#' which is for one grade. Also, one plot is saved in subfolder 'plot' inside
#' 'equating' folder for each grade.
#'
#' @param test Name of test.
#' @param grades Vector of all grades. Default is c(2:10).
#' @param forms Forms used in a test. Default is c('A','B')
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE.
#' @return List of chi-square test results on Form A and Form B for each grade.
#' @examples
#' Equate_Hrz(test='bang', grades=c(3, 5), long_label=T)
#' @export

Equate_Hrz <- function(test, grades=c(2:10), forms=c('A','B'), p_cut=0.05,
          DIF_cut=0.5, DIF_adj_cut=4, step=FALSE, iterative=FALSE){
  # grades, forms: combined in file names (Test_2A,Test_2B, ...)
  if (!dir.exists('equating')) dir.create('equating')

  grps <- list()
  for (i in seq_along(grades)){
    grps[[i]] <- str_c(grades[[i]], forms)
  }

  equat_ls <- list()
  for (i in seq_along(grps)){
    test_2 <- grps[[i]]
    cat('Equating Forms', test_2, '...\n')

    # extract facility and discrimination from its.xls file
    t1 <- paste0(test, '_', test_2[[1]])
    t2 <- paste0(test, '_', test_2[[2]])
    facilDiscrFitw <- df_its('output', t1) |>
      inner_join(
          df_its('output', t2),
          by = 'Label'
      ) |>
      dplyr::select(-contains('iNum.')) |>
      modify_at(c('Facil.x', 'Facil.y'), ~round(.x, 3)) |>
      na.omit()

    names(facilDiscrFitw) <- gsub("\\.x", paste0('_', test_2[[1]]), names(facilDiscrFitw))
    names(facilDiscrFitw) <- gsub("\\.y", paste0('_', test_2[[2]]), names(facilDiscrFitw))

    pstats <- plot_facilDiscrFitw(facilDiscrFitw, test_2, c(3, 7), 3)

    statsEqu <- Equate_shw(test, test_2, NULL, p_cut, DIF_cut,
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
    imap(~mutate(.x, Form=.y)) |>
    map2(map(equat_ls, 'flag'),
       ~mutate(
       .x,
       Links_bfr=nrow(.y),
       Links_afr=nrow(filter(.y, is.na(flag))),
       Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%')
       )
    ) |>
    reduce(bind_rows) |>
    select(Form, everything())
  sum_ls <- append(list(Summary=summary), map(equat_ls, 'flag'))

  # save results
  path_xlsx <- paste0('equating/', 'Hrz_', test, if(step) '_step', '.xlsx')
  writexl::write_xlsx(sum_ls, path_xlsx)

  path_pdf <- paste0('equating/', 'Hrz_', test, if(step) '_step', '.pdf')
  pdf(path_pdf, width=7, height=14)
  map(map(equat_ls, 'plot_DIF'), print)
  dev.off()

  # point users to files of varying purposes
  writeLines(c(
    paste0('Anchor DIF analysis for ', test, ' (before horizontal equating):'),
    paste0('\tSummary:\t', path_xlsx),
    paste0('\tPlots:\t\t', path_pdf)
  ))

  # sum_ls
}
