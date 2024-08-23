#' equate2Type
#'
#' This function performs vertical equating. An Excel file named 'Vrt_xxx.xlsx'
#' is saved in 'equating' folder, each sheet of which is for two
#' adjacent grades Also saved are scatterplots named 'Vrt_xxx.pdf'.
#'
#' @param linkTypeLst Length-one list such as list(Hrz=list(grades=c(2:10),
#' forms=c('A', 'B', 'C'))) or list(Vrt=c(2:10)). Name is Hrz or Vrt.
#' Element is list of grades and test forms or vector of grades.
#' @param test Name of test.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests. Default is 4.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iter TRUE to iteratively remove DIF items. Default is TRUE
#' @param sigma Indicator of how 'delta.y' is scaled. If TRUE, it is scaled
#' to have same mean and sd as 'delta.x'. If FALSE, it has same mean as 'delta.x'.
#' Default is FALSE.
#' @return List of chi-square test results for anchors between two adjacent grades.
#' @examples
#' equate2Type('Hrz', 'bang', 3)
#' @export

equate2Type <- function(linkTypeLst, test, p_cut=0.05, DIF_cut=0.5, DIF_std_cut=4,
                        step=FALSE, iter=TRUE, sigma=FALSE){
  # check inputs
  if (!is.list(linkTypeLst) || length(linkTypeLst)!=1 || !(names(linkTypeLst) %in% c('Hrz', 'Vrt'))) {
    stop('linkTypeLst should be a length-one list such as
        list(Hrz=list(grades=c(2:10), forms=c(\'A\', \'B\', \'C\'))) or
        list(Vrt=c(2:10)).
    Name is Hrz or Vrt.
    Element is list of grades and test forms or vector of grades.')
  }

  type <- names(linkTypeLst)
  if (type == 'Hrz') {
    if (!all(c('grades', 'forms') %in% names(linkTypeLst[[type]]))) {
      stop('linkTypeLst\'s element should be a list with names grades and forms! ')
    }
  }

  # specify variables used later

  if (type == 'Hrz') {
    grdIntVec <- linkTypeLst[[type]][['grades']]
    forms <- linkTypeLst[[type]][['forms']]
  } else {
    grdIntVec <- linkTypeLst[[type]]
  }

  var_name <- ifelse(type=='Hrz', '', 'L')

  folder <- paste0('equating/', type, '_', if(step) 'step_', test)
  if (!dir.exists(folder)) dir.create(folder)

  grps <- list()
  if (type=='Hrz') {
    for (i in seq_along(grdIntVec)){
      for (j in 1:(length(forms)-1)){
        nm <- paste0(grdIntVec[[i]], '_', paste0(forms[[j]], forms[[j+1]], collapse = ''))
        grps[[nm]] <- str_c(grdIntVec[[i]], c(forms[[j]], forms[[j+1]]))
      }
    }
  } else {
    for (i in 1:(length(grdIntVec)-1)){
      grps[[i]] <- c(grdIntVec[[i]], grdIntVec[[i+1]])
    }
  }

  equat_ls <- list()
  for (i in seq_along(grps)){
    vars <- grps[[i]]
    prefix <- paste0(folder, '/', paste0(vars, collapse='_'))

    # extract facility and discrimination from its.xls file
    t1 <- paste0(test, '_', vars[[1]])
    t2 <- paste0(test, '_', vars[[2]])

    if (!step) {
      facilDiscrFitw <- df_its('output', t1) |>
        inner_join(
          df_its('output', t2),
          by = 'Label'
        ) |>
        dplyr::select(-contains('iNum.')) |>
        na.omit() |>
        modify_at(c(3, 7), ~round(.x, 3))
      names(facilDiscrFitw) <- gsub(
          "\\.x",
          paste0(ifelse(type=='Hrz', ' ', ' L'), vars[[1]]),
          names(facilDiscrFitw)
        )
      names(facilDiscrFitw) <- gsub(
          "\\.y",
          paste0(ifelse(type=='Hrz', ' ', ' L'), vars[[2]]),
          names(facilDiscrFitw)
        )
    }

    # save scatterplots of delta and indice
    statsEqu <- Equate_shw(test, vars, var_name, p_cut, DIF_cut,
                           DIF_std_cut, FALSE, step, iter, sigma)
    ggsave(
      paste0(prefix, '_delta.png'),
      statsEqu[['plot_DIF']],
      width=17, height=30, units="cm"
    )

    if (!step) {
      ggsave(
        paste0(prefix, '_facilDiscrFitw.png'),
        plot_facilDiscrFitw(facilDiscrFitw, paste0(var_name, vars), c(3, 7), 3),
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
    }
    # statsEqu[['plot_DIF']] <- list(statsEqu[['plot_DIF']], pstats)
    # save Excel
    writexl::write_xlsx(
      statsEqu[1:5],
      paste0(prefix, '_process.xlsx')
    )

    equat_ls[[paste0(vars, collapse='_')]] <- statsEqu
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
  file <- paste0('equating/', type, '_', if(step) 'step_', test, '.xlsx')
  cat('\n', test, type, 'equating results saved at:\n\t', file)
  add_format()[['equate']](
    ls_save,
    folder,
    file,
    c(DIF_cut, DIF_std_cut)
  )

}
