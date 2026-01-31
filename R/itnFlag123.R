#' itnFlag123
#'
#' This function adds three priorities of flag to items in a test based on item
#' statistics. This is associated with test named 'test'.
#'
#' @param run String that indicates run such as 'pre_review' and 'post_review'.
#' @param test Name of test.
#' @param easy Threshold to flag easy items. Default is 90 (percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics. Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics. Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics. Default is 1.2.
#' @return Dataframe of item statistics with flags.
#' @examples
#' a <- itnFlag123(test='math_35')
#' @export

itnFlag123 <- function(run, test, easy = 90, hard = 10, iRst = .11,
                       fit_w = 1.1, fit_uw = 1.2){
  flag3 <- opt_stats(run, test) |>
    dplyr::filter(
      iScore==0,
      ptBis>0,
      `%correct`>10,
      !(resp %in% c(8:9, 'm', 'M'))
    ) |>
    dplyr::select(seqNo) |>
    unique() |>
    mutate(Flag=1)

  item_stats(run, test) |>
    left_join(flag3, by='seqNo') |>
    mutate(
      Priority = case_when(
        (Facility < hard | `Item-Rest Corr.` < 0) ~ 1,
        (`Item-Rest Corr.` < iRst | Infit > fit_w | Outfit > fit_uw) ~ 2,
        Flag == 1 ~ 3
      )
    ) |>
    dplyr::select(-Flag)
}
