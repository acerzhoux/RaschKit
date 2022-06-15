#' item_fit_opt_flag123
#'
#' This function adds three priorities of flag to items in a test based on item statistics. This is associated with test named 'test'.
#'
#' @param folder Folder that contains ConQuest output files associated with 'test'.
#' @param test Name of test.
#' @param easy Threshold to flag easy items. Default is 90 (means 90% correct).
#' @param hard Threshold to flag hard items. Default is 10 (means 10% correct).
#' @param iRst Threshold to flag low item-rest correlation statistics. Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics. Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics. Default is 1.2.
#' @return Dataframe of item statistics with flags.
#' @examples
#' item_fit_opt_flag123()

item_fit_opt_flag123 <- function(folder, test, easy=90, hard=10, iRst=.11,
                                 fit_w=1.1, fit_uw=1.2){
    item_fit_opt(folder=folder, test=test) %>%
        mutate(
            priority_3=priority_3(folder=folder, test=test),
            facilFlag=case_when(
                facil > easy ~ 'Very easy',
                facil < hard ~ 'Very difficult',
                TRUE ~ ''),
            pBisFlag=case_when(
                iRestCor < 0 ~ 'Negative discrimination',
                iRestCor < iRst ~ 'Near zero discrimination',
                iRestCor < .16 ~ 'Low discrimination',
                TRUE ~ ''),
            fitFlag=case_when(
                MNSQ_w > fit_w | MNSQ_uw > fit_uw ~ 'Poor fit',
                TRUE ~ ''),
            Priority=case_when(
                (facil < hard | iRestCor < 0) ~ '1',
                (iRestCor < iRst | MNSQ_w > fit_w | MNSQ_uw > fit_uw) ~ '2',
                priority_3 == TRUE ~ '3',
                TRUE ~ '')
        ) %>% select(-priority_3) %>%
        modify_at(c('facil', 'iRestCor', 'iTotCor', 'delta'),
                  ~as.numeric(.) %>%
                      round(digits=3))
}
