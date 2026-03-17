#' poly_recode
#'
#' This function down ranks non-continuous polytomous item response vectors
#' in a dataframe. If any item is recoded, a file with recoding ways is saved
#' into 'data' folder.
#'
#' @param test Name of the test.
#' @param respDf Dataframe with pid, covariables (e.g,, DIF variable), and
#' responses. Default is NULL where Excel file with name 'test' in 'data'
#' folder is used.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param n_cov Number of covariates before responses.
#' @param miss_code Missing codes. Default is c('r','R','m','M','9','x','X','.','',' ',NA).
#' @export

poly_recode <- function(test, keyDf, respDf, n_cov, miss_code=c('r','R','m','M','9','x','X','.','',' ',NA)){
  id_poly <- which(keyDf$Max_score > 1) + n_cov

  # save recoding ways to 'data' folder
  rcde <- map(respDf[id_poly], ~poly_recode_one(.x, miss_code = miss_code)[[2]])
  if (!all(map_lgl(rcde, is.null))){# generate string for CQ .cqc file
    iRecd <- Filter(Negate(is.null), rcde) |>
      imap(~mutate(.x, Item=.y)) |>
      reduce(bind_rows) |>
      select(Item, everything()) |>
      left_join(
        rowid_to_column(keyDf, 'iNum') |>
          select(iNum, Item),
        by='Item'
      )

    # save .csv file in 'data' folder for reference
    write.csv(
      iRecd,
      paste0('data/', test, '_recode_score.csv'),
      row.names=FALSE
    )

    # generate strings 'lab_cqc()' uses
    apply(
      iRecd[-1],
      1,
      function(x) {
        paste0('recode (', x[[1]], ') (', x[[2]], ') \t\t!items(', x[[3]], ');\n')
      }
    )
  } else {# no string produced
    NULL
  }

  # output recoded dataframe
  # modify_at(respDf, id_poly, ~poly_recode_one(.x, miss_code = miss_code)[[1]])
}
