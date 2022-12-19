#' DIF_steps_dich
#'
#' This function generates a tibble of steps to follow when performing DIF 
#' analysis on a dichotomous DIF variable.
#'
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @return Tibble of six steps to do DIF analyses on dichotomous variable.
#' @examples
#' # Not run
#' # DIF_steps_dich()
#' @export

DIF_steps_dich <- function(iterative=FALSE){
  tibble(
    Details = c('Calculate mean shift by subtracting Group 2\'s average delta estimates from that of Group 1.',
      'Adjust each delta estimate of Group 2 by adding to it the shift (Step 1).',
      'Calculate delta difference by subtracting each adjusted delta of Group 2 (Step 2) from that of Group 1.',
      'Adjust each delta difference (Step 3) by dividing it by adjusted error, i.e., sqrt(error.x^2 + error.y^2).',
      'Perform two-side df-1 Chi-squared test on each squared adjusted delta difference (Step 4).',
      if (iterative){
        'Iteratively perform Step 5 and each time remove the item of largest Chi-squared value until items all satisfy thresholds of p value, Chi-squared value, delta difference, and adjusted delta difference.'
      }),
    Steps = 1:length(Details)) %>%
        select(Steps, Details)
}
