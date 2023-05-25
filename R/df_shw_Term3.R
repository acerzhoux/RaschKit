#' df_shw_Term3
#'
#' This function extract item statistics from 'test_facet_shw.txt' file and add
#' significance test results and flags. This is associated with test named 'test'.
#'
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' @param test Name of test.
#' @examples
#' df_shw_Term3(DIFVar = 'Educator', test = 'RANZCOG')
#' @export

df_shw_Term3 <- function(DIFVar, test){
  folder <- paste0('DIF/', DIFVar)
  labs <- read.table(paste0('data/', test, '_Labels.txt')) |>
    rowid_to_column('item') |>
    dplyr::rename(`Item Title` = V1)
  test <- paste0(test, '_facet')

  # #### solve line difference issue arising from CQ versions
  n_item <- N_item(folder, test)
  lnT2 <- Lines(folder, test, 'shw', 'TERM 2: ')
  lnT3 <- Lines(folder, test, 'shw', 'TERM 3: item*')
  lessNum <- ifelse(length(lnT2)==1, 14, 13)
  n_cat <- lnT3[[length(lnT3)]] - lnT2[[length(lnT2)]] - lessNum
  nSkip <- ifelse(length(lnT2)==1, 7+n_item+10+n_cat+8, 8+n_item+11+n_cat+9)
  # #### End

  n_max <- {Lines(folder, test, 'shw', 'An asterisk ')[3] - 2} -
      {lnT3[[length(lnT3)]] + 6} + 1

  term3 <- readxl::read_xls(
      paste0(folder, '/', test, '_shw.xls'),
      sheet='ResponseModel',
      skip=nSkip,
      n_max=n_max+1,
      .name_repair = "unique_quiet"
    ) |>
    select(
      item,
      Category=3,
      Estimate=ESTIMATE,
      Error=`ERROR^`,
      Outfit=`MNSQ...7`,
      `Outfit T`=`T...10`,
      Infit=`MNSQ...11`,
      `Infit T`=`T...14`
    ) |>
    dplyr::filter(!is.na(item))

  smry <- term3 |>
    dplyr::select(item, !!sym(DIFVar) := Category, everything()) |>
    left_join(
      labs,
      by='item'
    ) |>
    arrange(item, !!sym(DIFVar)) |>
    mutate(
      Sig = ifelse(abs(Estimate)>1.96*Error, '*', NA),
      `Est>0.5` = ifelse(abs(Estimate)>0.5, '*', NA),
      `Est>1` = ifelse(abs(Estimate)>1, '*', NA),
      Flag = ifelse((Sig == '*' & `Est>0.5` == '*' & `Est>1` == '*'), 1, NA)
    ) |>
    dplyr::select(seqNo=item, `Item Title`, !!sym(DIFVar), everything())

  smry |>
    cbind(
      ` ` = '',
      `Note.` =  c(
        '\"Sig\" column checks whether the absolute DIF estimate',
        'is bigger than twice the measurement error associated with the estimate.',
        'If it isn\'t, then it isn\'t possible to conclude that the Educator really differs much from zero.',
        paste0('If any ', DIFVar, ' has Flag as \'1\' (Columns J K L all \"*\"), then there is probably a large effect for that item.'),
        rep('', nrow(smry)-4)
      )
    )
}
