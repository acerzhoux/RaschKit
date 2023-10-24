#' QA2Df
#'
#' This function checks whether two dataframes have same ID column and calculates
#' difference of each variable in the given name vector.
#'
#' @param id Unique ID column in both dataframes. This should be all same across
#' the two dataframes.
#' @param charVarVec Vector of char variables both dataframes contain. Equivalence
#' will be checked. Default is NULL.
#' @param numVarVec Vector of numeric variables both dataframes contain. Difference of
#' each variable in this vector will be calculated. Default is NULL.
#' @param selfDf Dataframe of your own.
#' @param otherDf Dataframe of others'.
#' @export

QA2Df <- function(id, charVarVec=NULL, numVarVec=NULL, selfDf, otherDf){
  # check input
  if (!all(c(charVarVec, numVarVec) %in% names(selfDf)) ||
    !all(c(charVarVec, numVarVec) %in% names(otherDf))) {
    stop('selfDf and otherDf should contain all variables in colNmVec!')
  }
  if (nrow(selfDf) != nrow(otherDf)) {
    stop('Dataframes should contain same number of rows!')
  }
  
  # order both dataframes
  selfDf <- selfDf |>
    dplyr::select(
      all_of(c(id, charVarVec, numVarVec))
    ) |>
    mutate(!!sym(id) := as.character(!!sym(id))) |>
    arrange(eval(parse(text=id)))
  
  otherDf <- otherDf |>
    dplyr::select(
      all_of(c(id, charVarVec, numVarVec))
    ) |>
    mutate(!!sym(id) := as.character(!!sym(id))) |>
    arrange(eval(parse(text=id)))
  
  # check if id are all same
  if (!all(selfDf[[id]] == otherDf[[id]])) {
    stop('Dataframes should have same id!')
  }
  
  # change NA in char variables to '.' to avoid NA for different values
  if (!is.null(charVarVec)) {
    selfDfChar <- selfDf[c(id, charVarVec)]
    selfDfChar[is.na(selfDfChar)] <- '.'
    selfDf <- left_join(
      selfDfChar,
      selfDf[c(id, numVarVec)],
      by=id
    )
    
    otherDfChar <- otherDf[c(id, charVarVec)]
    otherDfChar[is.na(otherDfChar)] <- '.'
    otherDf <- left_join(
      otherDfChar,
      otherDf[c(id, numVarVec)],
      by=id
    )
  }
  
  # save both dataframes, difference dataframe
  merged <- cbind(
    selfDf,
    ` `=rep('', nrow(selfDf)),
    otherDf,
    ` `=rep('', nrow(selfDf)),
    cbind(
      selfDf[, id],
      selfDf[charVarVec] == otherDf[charVarVec]
    )
  )
  
  if (!is.null(numVarVec)) {
    # only both NA is 0 difference
    difVec <- function(vec1, vec2) {
      difNum <- function(a, b) {
        if (is.na(a) & is.na(b)) {
          0
        } else if (is.na(a) & !is.na(b)) {
          b
        } else if (!is.na(a) & is.na(b)) {
          a
        } else {
          a-b
        }
      }
      map2_dbl(vec1, vec2, ~difNum(.x, .y))
    }
    
    merged <- cbind(
      merged,
      map(numVarVec, ~difVec(selfDf[[.x]], otherDf[[.x]])) |> 
        reduce(bind_cols, .name_repair='unique_quiet') |> 
        `names<-`(numVarVec)
    )
  }
  
  merged
}
