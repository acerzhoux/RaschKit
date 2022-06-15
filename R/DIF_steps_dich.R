#' DIF_steps_dich
#'
#' This function generates a tibble of steps to follow when performing DIF analysis on a dichotomous DIF variable.
#'
#' @return Tibble of six steps to do DIF analyses on dichotomous variable.
#' @examples
#' DIF_steps_dich()

DIF_steps_dich <- function(){
  tibble(Steps = 1:6,
    Details = c('Calculate mean shift by subtracting Category 2\'s average delta estimates from that of Category 1.',
      'Adjust each delta estimate of Category 2 by adding to it the shift (Step 1).',
      'Calculate delta difference by subtracting each adjusted delta of Category 2 (Step 2) from that of Category 1.',
      'Adjust each delta difference (Step 3) by dividing it by adjusted error, i.e., sqrt(error.x^2 + error.y^2).',
      'Perform two-side df-1 chi-square test on each squared adjusted delta difference (Step 4).',
      'Iteratively perform Step 5 and each time remove the item of largest chi-square value until items all satisfy thresholds of p value, chi-square value, delta difference, and adjusted delta difference.'))
}
