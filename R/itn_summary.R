#' itn_summary
#'
#' This function produces item analysis results with flags based on item statistics and CCC. This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 'test'.
#' @param test Name of test.
#' @param easy Threshold to flag easy items. Default is 90 (percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics. Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics. Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics. Default is 1.2.
#' @param dFallThr Ability on last bin above which falling distractor is flagged. Default is 0.5.
#' @param dRiseThr Ability on last bin below which rising distractor is unflagged. Default is 0.1.
#' @param ccc_data Data to draw CCC. One element of list output from Function 'CCC_Vernon'.
#' @param iType Dataframe with columns of iNum and itype. One element of list output from Function 'CCC_Vernon'.
#' @param quick TRUE when testing. Default is FALSE.
#' @return Dataframe of item statistics with flags.
#' @examples
#' itn_summary(test='AACA_202208_PRE_Meeting')
#' @export

itn_summary <- function(folder=here::here('output'), test,
                        easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2,
                        dFallThr=.5, dRiseThr=.1, ccc_data, iType, quick=FALSE){
    istats_flagged <- item_fit_opt_flag123(folder=folder, test=test, easy=easy,
                                           hard=hard, iRst=iRst,
                                           fit_w=fit_w, fit_uw=fit_uw)
    if (quick){
        istats_flagged <- istats_flagged %>%
            mutate(delta=delta-mean(delta))
    }

    # generate comments
    comments <- CCC_comments(folder=folder, test=test,
                             dFallThr=dFallThr, dRiseThr=dRiseThr,
                             ccc_data=ccc_data, iType=iType)

    #solve error in CCC_comments(): e.g., extra row from double key
    tryCatch({
        istats_flagged <- CCC_fine_flag4(iflag123=istats_flagged,
                                         comments=comments)
    },
    error = function(e) {
        istats_flagged <- istats_flagged %>%
            left_join(comments, by=c('qOrder'='iNum'))
    })

    # # remove comments on removed items
    # rm_id <- which(is.na(istats_flagged$delta))
    # rm_vars <- names(istats_flagged)[-c(1:3)]
    # istats_flagged[rm_id, rm_vars] <- NA

    # save results
    istats_flagged
}
