#' Equate_Vrt
#'
#' This function performs vertical equating. An Excel file named 'Vrt_xxx.xlsx'
#' is saved in 'equating' folder, each sheet of which is for two
#' adjacent grades Also saved are scatterplots named 'Vrt_xxx.pdf'.
#'
#' @param test Name of test.
#' @param grdIntVec Vector of all grades Default is c(2:10).
#' @param grdPrefix Character to add before grade in plots. Default is 'L'
#' (for 'level').
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iter TRUE to iteratively remove DIF items. Default is TRUE
#' @return List of chi-square test results for anchors between two adjacent grades.
#' @examples
#' Equate_Vrt(test='Math', grdIntVec=1:3)
#' @export

Equate_Vrt <- function(test, grdIntVec=c(2:10), grdPrefix='L', p_cut=0.05,
                       DIF_cut=0.5, DIF_adj_cut=4, step=FALSE, iter=TRUE){
  folder <- paste0('equating/Vrt_', test)
  if (!dir.exists(folder)) dir.create(folder)

  n_test <- length(grdIntVec)
  equat_ls <- list()
  for (i in 1:(n_test-1)){
    test_2 <- c(grdIntVec[[i]], grdIntVec[[i+1]])
    prefix <- paste0(folder, '/', paste0(test_2, collapse='_'))
    # cat('Equating grdIntVec',test_2,'...\n')

    # extract facility and discrimination from its.xls file
    t1 <- paste0(test, '_', test_2[[1]])
    t2 <- paste0(test, '_', test_2[[2]])
    facilDiscrFitw <- df_its('output', t1) |>
      inner_join(
        df_its('output', t2),
        by = 'Label'
      ) |>
      dplyr::select(-contains('iNum.')) |>
      na.omit() |>
      modify_at(c(3, 7), ~round(.x, 3))

    names(facilDiscrFitw) <- gsub("\\.x", paste0(' L', test_2[[1]]), names(facilDiscrFitw))
    names(facilDiscrFitw) <- gsub("\\.y", paste0(' L', test_2[[2]]), names(facilDiscrFitw))

    # save scatterplots of delta and indice
    statsEqu <- Equate_shw(test, test_2, grdPrefix, p_cut, DIF_cut,
                           DIF_adj_cut, FALSE, step, iter)
    ggsave(
      paste0(prefix, '_delta.png'),
      statsEqu[['plot_DIF']],
      width=17, height=30, units="cm"
    )
    ggsave(
      paste0(prefix, '_facilDiscrFitw.png'),
      plot_facilDiscrFitw(facilDiscrFitw, paste0('L', test_2), c(3, 7), 3),
      width=17, height=30, units="cm"
    )

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

    # statsEqu[['plot_DIF']] <- list(statsEqu[['plot_DIF']], pstats)
    # save Excel
    writexl::write_xlsx(
      statsEqu[1:5],
      paste0(prefix, '_process.xlsx')
    )

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
  ls_save <- list(Summary=summary) |>
    append(map(equat_ls, 'final'))

  # add format, hyperlink; save results
  file <- paste0('equating/Vrt_', test, '.xlsx')
  cat('\n', test, 'vertical equating results saved at:\n\t', file)
  add_format()[['equate']](
    ls_save,
    folder,
    file,
    c(DIF_cut, DIF_adj_cut)
  )

}
