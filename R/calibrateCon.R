#' calibrateCon
#'
#' This function merges grade datasets and codebook list, regress on grade, and
#' performs a free concurrent calibration. This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @param grdIntVec Integer vector of year levels. This is used with argument 'test'
#' to read data in 'data' folder.
#' @param cdLst List of dataframe (Item, Key, Max_score) for each year level.
#' @param pid Name of candidates' ID variable.
#' @param grdNm Variable name of Grade in data.
#' @param covNmVec Name vector of covariates before responses.
#' @export

calibrateCon <- function(test, grdIntVec, cdLst, pid, grdNm, covNmVec){
  # check
  if (!all(c(grdNm, pid) %in% covNmVec)){
    stop('grdNm and pid should be among covariates!')
  }

  n_cov <- length(covNmVec)
  dfLst <- list()
  for (i in 1:seq_along(grdIntVec)){
    grd <- grdIntVec[[i]]
    dfLst[[i]] <- haven::read_sav(paste0('data/', test, '_', grd, '.sav')) |>
      `names<-`(c(covNmVec, cdLst[[i]]$Item))
  }

  # data
  df0 <- reduce(dfLst, bind_rows)

  # key
  key0 <- cdLst[[1]]
  for (i in 2:length(cdLst)){
    key0 <- full_join(
      key0,
      cdLst[[i]],
      by=c('Key', 'Item')
    ) |>
    unique()
  }

  if (!all(key0$Item == names(df0)[-c(1:n_cov)])){
    stop('Merged data and merged key dataframe should have same item labels!')
  }

  calibrate(paste0(test, '_con'), df0, key0, pid, n_cov, grdNm)

  # can use read2one() to add format for itself
}
