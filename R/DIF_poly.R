#' DIF_poly
#'
#' This function performs DIF analysis on a polytomous variable and summarizes results.
#'
#' This functions saves a multiple-facet plot of plots for each subgroup's 
#' average delta (x) vs. subgroup delta (y) in the working directory's 'DIF' 
#' folder with the DIF variable in the name. Also saved in the 'DIF' folder is 
#' an Excel file with four sheets, 'comments', 'step', 'flags', and 'stats'.
#'
#' @param df_ls List of dataframes of test calibration results for various 
#' groups of the poloymous DIF variable. Each list element of the dataframe 
#' includes variables of item (item order in a single calibration of one 
#' DIF variable group), delta, and error.
#' @param DIFVar Name of polytomous DIF variable.
#' @param cats Vector of categories of the polytomous DIF variable, 
#' e.g., c(4, 5, 6, 7).
#' @param labels Dataframe of labels of all the items in a test with two 
#' variable of 'item' and 'label'. 'Item' is the item order in the test. 
#' 'Label' is the item label.
#' @param test Name of the test.
#' @param domain Name of the domain in the test, e.g., 'Literacy'. 
#' Default is NULL.
#' @param p_cut Threshold of statistical test. Default is 0.05.
#' @param step TRUE if polytomous items are involved. Default is FALSE.
#' @return List of summary of results from polytomous DIF variable analysis, 
#' including comments, step, summary statistics with flags, and statistics 
#' used to produce the the flags.
#' @examples
#' DIF_poly()
#' @export

DIF_poly <- function(df_ls, DIFVar, cats, labels, test, domain=NULL,
                     p_cut=0.05, step=FALSE){
    delta_cat_item <- map(df_ls, ~select(.x, item, delta)) %>%
        reduce(function(x, y) inner_join(x, y, by='item')) %>%
        `colnames<-`(c('item', str_c('delta_', cats))) %>%
        inner_join(labels, by='item') %>%
        select(-item) %>% rename(item=label)

    items <- delta_cat_item$item
    delta_cat <- delta_cat_item %>% select(-item)
    delta_cat_dif <- round(delta_cat[] - rowMeans(delta_cat[]), 3)

    # error
    error_cat_item <- map(df_ls, ~select(.x, item, error))%>%
        reduce(function(x, y) inner_join(x, y, by='item')) %>%
        inner_join(labels, by='item') %>%
        select(-item) %>% rename(item=label) %>%
        `colnames<-`(c(str_c('Error_',cats), 'item'))
    error_cat <- error_cat_item %>% select(-item)

    # #### Bonferroni adjusted normal distribution
    n_cat <- length(df_ls)
    t <- (delta_cat_dif / (error_cat*2)) %>% # standardized diff
        `colnames<-`(c(str_c('std_diff_', cats)))
    n_comp <- n_cat*(n_cat-1)/2
    threshold <- qnorm(1 - p_cut/n_comp/2, 0, 1) # Excel: NORMINV(p, m, s)
    Flag_dif_all <- map2(t, delta_cat_dif, ~DIF_symbol_poly(threshold, .x, .y))
    Flag_dif <- Flag_dif_all %>%
        map(~select(., '...1', '...2')) %>%
        map2(cats, ~`colnames<-`(.x, c(paste0('DIF_adj_', .y),
                                       paste0('sig_', .y)))) %>%
        reduce(bind_cols)
    delta_cat$delta_Ave <- round(rowMeans(delta_cat[]), 3)

    # plots
    plist <- map(1:length(cats), ~plot_DIF_poly(DIFVar, ., cats,
                           item=error_cat_item$item, delta_cat, Flag_dif))
    p_save <- patchwork::wrap_plots(plist, ncol=floor(sqrt(length(plist)))) +
        plot_annotation(title=paste0(str_to_title(test),
            if (!is.null(domain)) paste0(' ', domain),
            ': ', str_to_title(DIFVar), ' DIF Review on ', nrow(delta_cat), ' items')) +
        labs(x=paste0('Average Difficulty (Logits)'),
             caption=paste0('DIF: Bonferroni Adj. Sig. Test'))

    # summary of results
    DiF_sum <- cbind(items, {Flag_dif_all  %>%
            map(~select(., '...3')) %>%
            map2(cats, ~`colnames<-`(.x, .y)) %>%
            reduce(bind_cols)})
    comments <- DIF_comment_poly(DiF_sum = DiF_sum)

    # save results and plots
    sht <- paste0(DIFVar, '_', if(step) 'step_', test,
                  if (!is.null(domain)) paste0('_', domain))
    ggsave(here::here('DIF', paste0(sht, '.pdf')), p_save,
           width=20, height=20, units="cm")
    results <- list(comments = comments,
                    step = DIF_steps_poly(),
                    flags=cbind(items, Flag_dif),
                    stats=cbind(items, delta_cat, error_cat, round(t, 3)))
    writexl::write_xlsx(results, here::here('DIF', paste0(sht, '.xlsx')))

    # generate report
    output <- results %>%
        append(list(plot_DIF=p_save))

    # rmd_file <- here::here('rCode', 'report', 'DIF_poly.Rmd')
    rmd_file <- system.file("rmd", "DIF_poly.Rmd", package = "RaschKit")
    if (file.exists(rmd_file)) {
        rmarkdown::render(rmd_file,
                          params=list(output=output, test=test,
                                      DIFVar=DIFVar),
                          output_file=str_c(DIFVar, '_', test, '.html'),
                          output_dir=here::here('DIF'),
                          quiet=TRUE)
    }

    # point users to files of varying purposes
    writeLines(c(
        paste0('\n========= Output Files =========\n'),
        paste0(toupper(DIFVar), ' DIF analysis for ', test,
               if (step) ' (step)', ' (Bonferroni adjusted tests):'),
        paste0('\tSummary:\t', here::here('DIF', paste0(sht, '.xlsx'))),
        paste0('\tDIF Plot:\t', here::here('DIF', paste0(sht, '.pdf'))),
        if (file.exists(rmd_file)){
            paste0('\tDIF report:\t', here::here('DIF', str_c(DIFVar, '_', test, '.html')))
        }
    ))

    output
}
