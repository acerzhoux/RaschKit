#' DIF_steps_poly
#'
#' This function generates a tibble of steps to follow when performing a type of DIF analysis on a polytomous DIF variable.
#'
#' @return Tibble of four steps to do DIF analyses.
#' @examples
#' DIF_steps_poly()
#' @export

DIF_steps_poly <- function(){
  tibble(Steps = 1:4,
  Details = c('Separately calibrate all items for each category of the DIF variable.',
    'For each item in each calibration (Step 1) subtract the average difficulty estimate of all categories from each category.',
    'Standardize each category\'s deviation from the average (Step 2) by dividing it by squared standard error of each difficulty estimate in each calibration (Step 1).',
    'Consider the standardized estimate as Bonferroni adjusted normal distribution ~ N(0, 1) with degree of freedom of n(n-1)/2 where n is the number of categories of the DIF variable. Then, the p value of the two-sided significant test will be 0.05 divided by n(n-1)/4. The standardized deviation is significant when its absolute value is bigger than the z value at the adjusted p level.'))
}
