#' itn_summary
#'
#' This function produces item analysis results with flags based on item
#' statistics and CCC. This is associated with test named 'test'.
#'
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
#' @param quick TRUE when testing. Default is FALSE.
#' @return Dataframe of item statistics with flags.
#' @examples
#' itn_summary(test='AACA_202208_PRE_Meeting')
#' @export

itn_summary <- function(test, easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2,
                        dFallThr=.5, dRiseThr=.1, ccc_data, iType, quick=FALSE){
    istats_flagged <- item_fit_opt_flag123(test, easy, hard, iRst, fit_w, fit_uw)
    if (quick) istats_flagged <- mutate(istats_flagged, delta = delta-mean(delta))

    # generate comments
    comments <- CCC_comments(test, dFallThr, dRiseThr, ccc_data, iType)

    #solve error in CCC_comments(): e.g., extra row from double key
    tryCatch({
        istats_flagged <- CCC_fine_flag4(istats_flagged, comments)
    },
    error = function(e) {
        istats_flagged <- left_join(
            istats_flagged,
            comments,
            by = c('qOrder'='iNum')
        )
    })

    # save results
    istats_flagged
}
