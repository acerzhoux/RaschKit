#' DIF_steps_dich_step
#'
#' This function generates a tibble of steps to follow when performing DIF
#' analysis on a dichotomous DIF variable and polytomous responses.
#'
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @return Tibble of six steps to do DIF analyses on dichotomous variable and
#' polytomous responses.
#' @examples
#' # Not run
#' # DIF_steps_dich_step(iterative=TRUE)
#' @export

DIF_steps_dich_step <- function(iterative=FALSE){
  tibble(
    Details = c('Multiply each step estimate\'s error in both groups by sqrt(desig_effect).',
      'Adjust each step estimate by subtracting from it the group mean of step estimates.',
      'Calculate step difference by subtracting each adjusted step of Group 2 (Step 2) from that of Group 1 (Step 2).',
      'Divide each step difference (Step 3) by pooled standard error, i.e., sqrt(error.x^2 + error.y^2) (Step 1).',
      'Perform two-tailed df-1 Chi-squared test on each squared standardized step difference (Step 4).',
      if (iterative){
        'Iteratively perform Step 5 and each time remove the step of largest Chi-square until steps all satisfy thresholds of p value, absolute step difference, and absolute standardized step difference.'
      }),
    Steps = 1:length(Details)) %>%
        select(Steps, Details)
}
