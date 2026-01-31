#' equiva_tbl
#'
#' This function extracts desired type of ability estimates from compressed
#' ConQuest .cqs file and generate scaled scores with specified slope and
#' intercept. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @param est_type Type of ability estimate to use for score equivalence table,
#' 'wle' or 'mle'. Default is 'wle'.
#' @param slope Slope to multiply ability estimates. Default is NULL
#' @param intercept Value/intercept to add to ability estimates. Default is NULL.
#' @param extrapolation Whether to extrapolate the minimum and maximum estimates.
#' Default is FALSE.
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @return Dataframe of scaled ability estimates.
#' @examples
#' equiva_tbl(test='Writing')
#' @export

equiva_tbl <- function(test, est_type='wle', slope=NULL, intercept=NULL,
                       extrapolation=FALSE, run){
  path.out <- file.path('calibration', run, sprintf('%s_eqv.txt', test))
  path.in <- file.path('calibration', run, sprintf('%s_compressed.CQS', test))

  cn <- c(
    'reset;',
    sprintf('get << %s;', path.in),
    paste0('equivalence ', est_type, ' >> ', path.out, ';'),
    'reset all;',
    'quit;'
  )
  cqc_path <- file.path('input', run, sprintf('%s_eqv.cqc', test))
  writeLines(cn, cqc_path)

  # call CQ and generate equiv table
  conquestr::ConQuestCall(
    cqc = cqc_path,
    cqExe = file.path('C:', 'Program Files', 'ACER ConQuest', 'ConQuestConsole.exe'),
    stdout = NULL
  )

  # read equiv tbl
  lines <- readLines(path.out)
  ind2 <- grep('=====', lines)[[2]]-1
  eqv_tbl <- read.table(text=str_replace_all(lines[11:ind2], '_BIG_', ' NA')) |>
    as_tibble() |>
    `colnames<-`(c('Score_raw','EST','SE'))

  # extrapolation
  if (extrapolation){
    tbl_r <- nrow(eqv_tbl)-1
    est_min <- eqv_tbl[2:4,]$EST
    est_min <- est_min[[3]] - est_min[[2]]*3 + est_min[[1]]*3
    est_max <- eqv_tbl[(tbl_r-2):tbl_r,]$EST
    est_max <- est_max[[3]]*3 - est_max[[2]]*3 + est_max[[1]]

    eqv_tbl[1, ]$EST <- est_min
    eqv_tbl[(tbl_r+1), ]$EST <- est_max
  }

  # scale scores
  if (!is.null(intercept) | !is.null(slope)){
    cat('Generating scaled-score table...\n')
    if (is.null(intercept)) intercept <- 0
    if (is.null(slope)) slope <- 1
    eqv_tbl <- eqv_tbl |>
      mutate(Score_scale=round(slope*EST + intercept, digits=0))
  }
  # save results
  path.out <- file.path('calibration', run, sprintf('eqv_%s.xlsx', test))
  eqv_tbl |>
    mutate(Score_raw=round(Score_raw)) |>
    writexl::write_xlsx(path.out)
}
