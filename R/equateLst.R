#' equateLst
#'
#' This function returns a list of functions to add format such as header and
#' body style to Excel sheet.
#'
#' @param deltaDfLst List of tests' anchor statistics dataframe of variables 'item',
#' 'delta.x', 'error.x', 'delta.y', 'error.y'. The names of the elements are
#' test names.
#' @param vars Vector of length 2 such as c('2022','2023'). Its order corresponds
#' to two tests associated with .x and .y.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5. Give vector of cuts if tests vary in cut.
#' @param DIF_std_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4. Give vector of cuts if tests vary in cut.
#' @param design_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param iter TRUE to iteratively remove DIF items. Default is TRUE.
#' @param indDfLst List of tests' indice statistics dataframe of variables 'item',
#' 'facil.x', 'discr.x', 'fitw.x', 'facil.y', 'discr.y', 'fitw.y'. Those indice
#' variable should be in the order above and can be incomplete. The names of
#' the elements are test names and should be in same order as deltaDfLst. Default
#' is NULL when no index is available.
#' @return Summary file of chi-square test results on tests in list with plots,
#' hyperlinks, flag color, and format.
#' @export

equateLst <- function(deltaDfLst, vars, p_cut=0.05, DIF_cut=0.5, DIF_std_cut=4,
                       design_effect=1, step=FALSE, iter=TRUE, indDfLst=NULL){
  # check input
  tests <- names(deltaDfLst)
  if (is.null(tests)) {
    stop('deltaDfLst should have test name for each Df element!')
  }

  lenCheck <- map_int(deltaDfLst, nrow) == map_int(deltaDfLst, ~nrow(na.omit(.x)))
  if (any(!lenCheck)) {
    stop(paste0(names(lenCheck[!lenCheck]), ' contains missing values! Remove and retry.'))
  }

  if (length(DIF_cut)!=1 & length(DIF_cut)!=length(tests)) {
    stop('Number of DIF_cut should equal number of tests!')
  }

  if (length(DIF_std_cut)!=1 & length(DIF_std_cut)!=length(tests)) {
      stop('Number of DIF_std_cut should equal number of tests!')
  }

  # check DIF_cut, DIF_std_cut
  if (length(DIF_cut)==1) DIF_cut <- rep(DIF_cut, length(tests))
  if (length(DIF_std_cut)==1) DIF_std_cut <- rep(DIF_std_cut, length(tests))

  # folders, file names
  subfolder <- ifelse(
      step,
      paste0('step_', vars[[1]], ' vs ', vars[[2]]),
      paste0(vars[[1]], ' vs ', vars[[2]])
    )
  folder <- paste0('equating/', subfolder)
  if (!dir.exists(folder)) dir.create(folder)

  # DIF analysis
  if (is.null(indDfLst)) {
    for (i in seq_along(tests)){
      Equate(deltaDfLst[[i]], tests[[i]], vars, p_cut, DIF_cut[[i]], DIF_std_cut[[i]], TRUE,
           design_effect, step, NULL, iter)
    }
  } else {
    for (i in seq_along(tests)){
      Equate(deltaDfLst[[i]], tests[[i]], vars, p_cut, DIF_cut[[i]], DIF_std_cut[[i]], TRUE,
             design_effect, step, NULL, iter, indDfLst[[i]])
    }
  }

  # extract stats
  file_ls <- map(tests, ~list.files(folder, pattern=.x, full.names=TRUE))
  files <- file_ls |>
    map(~str_subset(.x, '.xlsx')) |>
    unlist()
  ex_ls <- map(files, ~readxl::read_xlsx(.x, 'final'))

  # combine excels
  names(ex_ls) <- tests
  summary <- map(files, ~readxl::read_xlsx(.x, 'shift'))  |>
    map2(tests, ~mutate(.x, Domain=.y)) |>
    map2(map(files, ~readxl::read_xlsx(.x, 'final')),
         ~mutate(.x, Links_bfr=nrow(.y),
                 Links_afr=nrow(filter(.y, flag==0)),
                 Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%'))) |>
    reduce(bind_rows) |>
    select(Domain, everything())
  ls_save <- list(Shift=summary) |>
    append(ex_ls)

  # add format
  file <- paste0(folder, ".xlsx")
  add_format()[['equate']](
    ls_save,
    folder,
    file,
    list(DIF_cut, DIF_std_cut)
  )

  cat('\nEquating summary file is at:\n\t', file)

}
