#' itn_summary
#'
#' This function produces item analysis results with flags based on item
#' statistics and CCC. This is associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @param easy Threshold to flag easy items. Default is 90 (percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics.
#' Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics.
#' Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics.
#' Default is 1.2.
#' @param dFallThr Ability on last bin above which falling distractor is flagged.
#' Default is 0.5.
#' @param dRiseThr Ability on last bin below which rising distractor is unflagged.
#' Default is 0.1.
#' @param ccc_data Data to draw CCC. One element of list output from Function 'CCC_Vernon'.
#' @param iType Dataframe with columns of iNum and itype. One element of list
#' output from Function 'CCC_Vernon'.
#' @param keyDf Dataframe of 'Item', 'Key', and 'Max_score' (add Key2 if double key).
#' @param anchor TRUE if anchoring is being done.
#' @return Dataframe of item statistics with flags.
#' @examples
#' a <- itn_summary(test='FPA', ccc_data=ccc_data, iType=iType, keyDf=keyDf)
#' @export

itn_summary <- function(run, test, easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2,
                        dFallThr=.5, dRiseThr=.1, ccc_data, iType, keyDf, anchor){
  # process double keys
  if (('Key2' %in% names(keyDf)) && any(!is.na(keyDf$Key2))) {
    ks <- names(select(keyDf, contains('Key')))
    keyDf <- keyDf |>
      dplyr::mutate(Key=apply(keyDf[ks], 1, function(x) paste0(sort(na.omit(x)), collapse='')))
  }

  # put together summary
  sum.tbl <- right_join(
      keyDf,
      CCC_fine_flag4(
        itnFlag123(run, test, easy, hard, iRst, fit_w, fit_uw),
        CCC_comments(test, dFallThr, dRiseThr, ccc_data, iType)
      ) |>
        mutate(`Item Title`=as.character(`Item Title`)),
      by=c('Item'='Item Title')
    ) |>
    bind_cols(
      tibble(
        `Files`=c(
          paste0(test, '_CCC.pdf'),
          paste0(test, '_ipMap.pdf'),
          paste0(test, '_Convergence_check.pdf'),
          paste0(test, '_Frequency_check.xlsx'),
          rep(NA, nrow(keyDf)-4)
        )
      ),
    ) |>
    dplyr::mutate(
      Test=test,
      ICC=paste0(test, '_CCC.pdf')
    ) |>
    dplyr::select(
      Test,
      seqNo,
      ICC,
      `Item Title`=Item,
      Key,
      `Max Score`=Max_score,
      contains('Item Estimate'),
      `Item Error`,
      Facility,
      `Item-Rest Corr.`,
      `Item-Total Corr.`,
      Outfit,
      Infit,
      `C.I. (L)`,
      `C.I. (H)`,
      `T`,
      `Number of Students`,
      Priority,
      Comment,
      Files
    ) |>
    dplyr::mutate(
      Priority=ifelse(is.na(`Item Estimate (item centred)`), NA, Priority),
      Comment=ifelse(is.na(`Item Estimate (item centred)`), 'No data.', Comment),
      Key=ifelse(`Max Score`>1, 'CR', Key),
      `Max Score`=ifelse(is.na(`Item Estimate (item centred)`), NA, `Max Score`),
      Facility=ifelse(is.na(`Item Estimate (item centred)`), NA, Facility)
    )

  if (anchor){
    sum.tbl <- sum.tbl |>
      rename(`Item Estimate (anchored)`=`Item Estimate (case centred)`) |>
      dplyr::select(-`Item Estimate (item centred)`)
  }

  if ('Item Estimate (case centred)' %in% names(sum.tbl)){
    sum.tbl <- sum.tbl |>
      dplyr::select(-`Item Estimate (case centred)`)
  }

  return(sum.tbl)
}
