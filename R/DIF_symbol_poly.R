#' DIF_symbol_poly
#'
#' This function is called by function DIF_poly_df() to perform Bonferroni significance test of difference between each subgroup's delta and the average delta of all subgroups.
#'
#' @param cutscore Bonferroni adjustment of statistical significance value p (e.g., 0.05) of a two-sided test. The adjusted p value is p/(n*(n-1)/2)/2 where n is number of categories of the polytomous DIF variable.
#' @param t_vec Vector of standardized difference between a subgroup's items' delta estimates and the average of all subgroups' items' delta estimates. 'standardized' means that the delta difference of each item in each subgroup is divided by the square of corresponding error of that delta.
#' @param cat_vec Vector of difference between a subgroup's items' delta estimates and the average of all subgroups' items' delta estimates.
#' @return Tibble of Bonferroni adjusted statistical significance results of three variables/columns. Column 1 is that subgroup's item delta difference, symbol of significance ('*') or not, symbol of bigger than delta mean ('B') or smaller ('S').
#' @examples
#' DIF_symbol_poly()

DIF_symbol_poly <- function(cutscore, t_vec, cat_vec){
    cutscore <- abs(cutscore)
    cat_vec_sig <- vector('character', length(cat_vec))
    cat_vec_sign <- vector('character', length(cat_vec))
    for (i in seq_along(cat_vec)){
        if (cat_vec[i] > 0) {
            if (t_vec[i] > cutscore) {
                cat_vec_sig[i] <- '*'
                cat_vec_sign[i] <- 'B'
            }
        } else {
            if (t_vec[i] < -cutscore) {
                cat_vec_sig[i] <- '*'
                cat_vec_sign[i] <- 'S'
            }
        }
    }
    bind_cols(cat_vec, cat_vec_sig, cat_vec_sign)
}
