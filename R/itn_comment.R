#' itn_comment
#'
#' This function describes meaning of four priorities of item examination.
#'
#' @export

itn_comment <- function(){
  tibble(
    Priority=1:4,
    Description=c(
      'Very difficult item (Facility below 10) or negative Item-Rest correlation (Item-Rest below 0). These criteria are designed to identify any possible miskeyed items.',
      'Item-Rest near zero (Item-Rest below 0.11) or poor item fit (Outfit above 1.2 or Infit above 1.1). These criteria are designed to identify any item where there may be relatively higher proportions of unexpected responses given candidates’ abilities.',
      'Any item distractor has positive Item-Rest correlation, and the proportion of candidates selecting this distractor is over 10%.',
      'Additional items flagged after reviewing Category Characteristic Curve (CCC) plots for any potential issues. Items listed in this fourth priority group alone are identified only on the basis of CCC examination.'
    )
  )
}
