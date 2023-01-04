#' Equate
#'
#' This function performs chi-square tests (DIF analysis) on a group of anchor 
#' items' difference of delta estimates in two tests. An Excel file with test 
#' results and flags can be saved in 'equating' folder. Also, one plot is saved 
#' in subfolder 'plot' inside 'equating' folder.
#'
#' @param df Dataframe of five variables of 'item' (anchor labels), 'delta.x', 
#' 'error.x', 'delta.y', and 'error.y'. '.x' and '.y' should correspond to order 
#' of test names in 'vars'.
#' @param test Name of test such as 'AGAT'.
#' @param vars Vector of length 2 such as c('VIC','NSW'). Its order corresponds 
#' to two tests associated with .x and .y.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param chi_cut Threshold of chi-square difference between two tests. 
#' Default is 10.
#' @param DIF_cut Threshold of an item's delta estimate difference between two 
#' tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference 
#' between two tests. Default is 4.
#' @param sav_results TRUE if an Excel file with chi-square test results and 
#' a plot are desired. Default is TRUE.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. 
#' Default is FALSE.
#' @param DIF TRUE if DIF analysis is performed on a dichotomous DIF variable. 
#' Default is FALSE (anchor check).
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @return Dataframe of chi-square test results for anchors between two tests.
#' @examples
#' Equate(df=data[, c('item', 'delta.x', 'error.x', 'delta.y', 'error.y')], 
#' test='Elana_math', vars=c('NSW', 'VIC'))
#' @export

Equate <- function(df, test, vars, p_cut=0.05, chi_cut=10,
                   DIF_cut=0.5, DIF_adj_cut=4, sav_results=TRUE,
                   desig_effect=1, step=FALSE, DIF=FALSE,
                   iterative=FALSE){
    folder <- here::here('equating')
    if (!dir.exists(folder)) dir.create(folder)

    # DIF check
    if (step){
        results <- chi_square_test_step(df=df, desig_effect=desig_effect)
    } else {
        results <- chi_square_test(df=df)
    }

    ax_min <- min(results$delta.x, results$delta.y_adj)
    ax_max <- max(results$delta.x, results$delta.y_adj)

    shift <- tibble(cor_bfr = round(cor(results$delta.x, results$delta.y_adj), 3),
                    shift_bfr = round(mean(results$delta.x)-mean(results$delta.y), 3),
                    sdr_bfr = round(sd(results$delta.y)/sd(results$delta.x), 3))

    p1 <- plot_DIF(df=error_band(results), wh='Before', vars=vars,
                   p_cut=p_cut, chi_cut=chi_cut, DIF_cut=DIF_cut,
                   DIF_adj_cut=DIF_adj_cut, step=step, DIF=DIF,
                   cor=shift$cor_bfr, shift=shift$shift_bfr, sdr=shift$sdr_bfr,
                   ax_min=ax_min, ax_max=ax_max)

    updated <- results
    iDIF <- DIF_items(df=updated, p_cut=p_cut, chi_cut=chi_cut,
                      DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut)

    # two ways of dealing with DIF items
    if (iterative){
        # iteraively remove DIF anchor of max chi-sq
        while (dim(iDIF)[1] != 0){
            if (step){
                updated <- chi_square_test_step(df={updated %>%
                        filter(chisq!=max(chisq))},
                        desig_effect=desig_effect)
            } else {
                updated <- chi_square_test(df={updated %>%
                        filter(chisq!=max(chisq))})
            }
            iDIF <- DIF_items(df=updated, p_cut=p_cut, chi_cut=chi_cut,
                              DIF_cut=DIF_cut, DIF_adj_cut=DIF_adj_cut)
        }
    } else {
        # filter once with all conditions
        if (nrow(iDIF)==0){

        } else {
            updated <- updated %>%
                filter(!(item %in% iDIF$item))
        }
    }

    shift <- shift %>%
        mutate(cor_afr = round(cor(updated$delta.x, updated$delta.y_adj), 3),
               shift_afr = round(mean(updated$delta.x)-mean(updated$delta.y), 3),
               sdr_afr = round(sd(updated$delta.y)/sd(updated$delta.x), 3))

    # plot non-DIF anchors
    p2 <- plot_DIF(df=error_band(updated), wh='After', vars=vars,
                   step=step, DIF=DIF,
                   cor=shift$cor_afr, shift=shift$shift_afr, sdr=shift$sdr_afr,
                   ax_min=ax_min, ax_max=ax_max)
    p_save <- p1 / p2 +
        plot_annotation(title=paste0('Number of DIF ', if(step) 'step ' else 'Items ', 'in ',
                                     toupper(test), ': ', nrow(results)-nrow(updated)),
                        subtitle=paste0(vars[[1]], ' vs. ', vars[[2]]),
                        tag_levels='I')

    # DIF anchors found
    if(step) {
        iDIF <- setdiff(results$iStep, updated$iStep)
        results_flag <- results %>%
            mutate(flag=ifelse(iStep %in% iDIF, 1, NA))
    } else {
        iDIF <- setdiff(results$item, updated$item)
        results_flag <- results %>%
            mutate(flag=ifelse(item %in% iDIF, 1, NA))
    }

    # change .x, .y in names
    results_flag <- results_flag %>%
        `names<-`(gsub("\\.x", str_c('_', vars[[1]]), names(.))) %>%
        `names<-`(gsub("\\.y", str_c('_', vars[[2]]), names(.)))
    updated <- updated %>%
        `names<-`(gsub("\\.x", str_c('_', vars[[1]]), names(.))) %>%
        `names<-`(gsub("\\.y", str_c('_', vars[[2]]), names(.)))

    output <- list(comments=DIF_comment_dich_equate(vars=vars, iDIF=iDIF, DIF=DIF),
                  step=if (step) DIF_steps_dich_step(iterative=iterative) else DIF_steps_dich(iterative=iterative),
                  shift=shift,
                  flag=results_flag,
                  final=updated,
                  plot_DIF=p_save)

    if (sav_results) {
        # save results and plots
        sht <- paste0(test, '_',
                      if(step) 'step_', vars[[1]], ' vs ', vars[[2]])
        path_xlsx <- file.path(folder, paste0(sht, '.xlsx'))
        writexl::write_xlsx(output[1:5], path_xlsx)
        path_pdf <- file.path(folder, paste0(sht, '.pdf'))
        ggsave(path_pdf, p_save, width=17, height=30, units="cm")

        # rmd_file <- here::here('rCode', 'report', 'Equating_dich.Rmd')
        rmd_file <- system.file("rmd", "Equating_dich.Rmd", package = "RaschKit")

        if (file.exists(rmd_file)){
            rmarkdown::render(rmd_file,
                              params=list(test=test,vars=vars,
                                          step=step, DIF=DIF,
                                          output=output),
                              output_file=str_c(sht, '.html'),
                              output_dir=here::here('equating'),
                              quiet=TRUE)
        }

        # point users to files of varying purposes
        writeLines(c(
            paste0('\n========= Output Files =========\n'),
            paste0('Anchor DIF analysis for ', test,
                   ' (', vars[[2]], ' vs. ', vars[[1]], '):'),
            paste0('\tSummary:\t', path_xlsx),
            paste0('\tPlot:\t\t', path_pdf),
            if (file.exists(rmd_file)){
                paste0('\tReport:\t\t', file.path(folder, str_c(sht, '.html')))
            }
        ))
    } else {
        output
    }
}
