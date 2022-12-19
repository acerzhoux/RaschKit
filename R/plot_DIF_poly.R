#' plot_DIF_poly
#'
#' This function plots all items' average delta estimates against those in each 
#' subgroup. It also labels items that showed DIF in the Bonferroni adjusted 
#' significance test.
#'
#' @param DIFVar Name of polytomous DIF variable.
#' @param subgrp Order of a category of the DIF variable among all categories. 
#' Minimum is 1. Maximum is length of categories.
#' @param cats Ordered categories of the DIF variable, e.g., c(4:10)
#' @param item Vector of item labels that had estimates in each subgroup.
#' @param delta_cat Dataframe with items' delta estimates in subgroups (1st to 
#' last but one columns) and average estimates (last column).
#' @param Flag_dif Dataframe with both adjusted delta estimates (one column) 
#' and significant symbols (one column) for each subgroup.
#' @return Plot of DIF analysis of test items (that had estimates for all 
#' subgroups) on one subgroup.
#' @examples
#' plot_DIF_poly()
#' @export

plot_DIF_poly <- function(DIFVar, subgrp, cats, item, delta_cat, Flag_dif) {
    df <- data.frame(item=item,
                     average=delta_cat[ncol(delta_cat)],
                     subgroup=delta_cat[subgrp])
    names(df) <- c('item', 'average', 'subgroup')

    txt1 <- tibble(average=-Inf,
                   subgroup=Inf,
                   label=paste0('Harder for ', DIFVar, ' ', cats[[subgrp]]))
    txt2 <- tibble(average=Inf,
                   subgroup=-Inf,
                   label='Easier')
    flags <- item[which(Flag_dif[[subgrp*2]] == '*')]
    DIF_txt <- df %>%
        filter(item %in% flags)

    ggplot(df, aes(average, subgroup)) +
        geom_point() +
        geom_smooth(method='lm') +
        geom_text(aes(label=label), data=txt1, vjust='top', hjust='left') +
        geom_text(aes(label=label), data=txt2, vjust='bottom', hjust='right') +
        labs(x='', y=paste(DIFVar, cats[[subgrp]], 'Difficulty' )) +
        ggthemes::theme_tufte() +
        ggrepel::geom_label_repel(data=DIF_txt, aes(label=item),
                                  size=2, segment.size=0.25, alpha=0.5)
}
