#' DIF_dich
#'
#' This function performs chi-square tests (DIF analysis) on all items's
#' difference of delta estimates between two groups of test takers.
#' An Excel file with test results and flags is saved in 'DIF' folder.
#' Also, one plot is saved in subfolder 'plot' inside 'DIF' folder.
#'
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param test Name of test.
#' @param vars Vector of length 2 such as c('girls','boys'). Its order
#' corresponds to the alphabetic/numeric order of DIF variables' two categories in data.
#' @param df Dataframe with delta estimates and errors of items for both
#' categories of dichomomous variable in a test.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between
#' two tests. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two tests. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. Default is FALSE.
#' @param facil_cut Threshold of number of percent to flag an item with large
#' facility difference between two groups of test takers. Default is 10.
#' @param iterative TRUE to iteratively remove DIF items. Default is FALSE
#' @param save_xlsx Whether to save summary file and plots. Default is TRUE
#' (one DIF variable).
#' @param quick Whether quick error is needed. Default is TRUE for DIF analysis.
#' @return List of summary of results from dichotomous DIF variable analysis,
#' including comments, step, summary statistics with flags, and statistics of
#' items after review.
#' @examples
#' # Not run
#' # DIF_dich(DIFVar='gender', test='ELNA', vars=c('Girls', 'Boys'), df=df)
#' @export

DIF_dich <- function(DIFVar, test, vars, df,
                     p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4,
                     desig_effect=1, step=FALSE, facil_cut=10,
                     iterative=FALSE, save_xlsx=TRUE,
                     quick=TRUE){
    # function to get range of x, y axis
    getRange <-  function(data, step){
        if (step){
            range(data[['delta.x_dev']], data[['delta.y_dev']]) + c(-.3, .3)
        } else {
            range(data[['delta.x']], data[['delta.y_adj']]) + c(-.3, .3)
        }
    }

    if (!dir.exists('DIF')) dir.create('DIF')

    # read N and facilities from .its file
    N_facil <- file_its(test=test, DIFVar=DIFVar) |>
        mutate(X1=str_squish(X1)) |>
        separate(X1, str_c('V', 1:8), ' ') |>
        mutate(item=parse_number(V3),
               N=parse_number(V4),
               item=str_replace(item, '\\(', ''),
               var=str_sub(V1, -1, -1),
               var=ifelse(var==1, vars[[1]], vars[[2]])) |>
        select(var, item, N, Facil=V5) |>
        pivot_wider(
            names_from = var,
            values_from = N:Facil
        ) |>
        modify_at(-1, as.numeric) |>
        modify_at(4:5, ~round(., digits = 3))

    # DIF: delta
    results <- chi_square_test(df)
    iDIF <- DIF_items(results, p_cut, DIF_cut, DIF_adj_cut) |>
        pull(item)

    # DIF check
    if (step){
        results <- chi_square_test_step(df=df, desig_effect=desig_effect)
        shift <- tibble(cor_bfr = round(cor(results$delta.x_dev, results$delta.y_dev), 3),
                        shift_bfr = round(mean(results$delta.x_dev)-mean(results$delta.y_dev), 3),
                        sdr_bfr = round(sd(results$delta.y_dev)/sd(results$delta.x_dev), 3))
    } else {
        results <- chi_square_test(df=df)
        shift <- tibble(cor_bfr = round(cor(results$delta.x, results$delta.y_adj), 3),
                        shift_bfr = round(mean(results$delta.x)-mean(results$delta.y), 3),
                        sdr_bfr = round(sd(results$delta.y)/sd(results$delta.x), 3))
    }

    p1 <- plot_DIF(error_band(results), 'Before', vars, p_cut, DIF_cut,
             DIF_adj_cut, step, TRUE, shift$cor_bfr, shift$shift_bfr,
             shift$sdr_bfr, getRange(results, step), quick)


    # iteraively remove DIF anchor of max chi-sq
    updated <- results
    iDIF <- DIF_items(results, p_cut, DIF_cut, DIF_adj_cut)

    # two ways of dealing with DIF items
    if (iterative){
        # iteraively remove DIF item of max chi-sq
        while (dim(iDIF)[1] != 0){
            if (step){
                updated <- chi_square_test_step(df={updated |>
                        filter(abs(DIF_std)!=max(abs(DIF_std)))},
                        desig_effect=desig_effect)
            } else {
                updated <- chi_square_test(df={updated |>
                        filter(abs(DIF_std)!=max(abs(DIF_std)))})
            }
            iDIF <- DIF_items(updated, p_cut, DIF_cut, DIF_adj_cut)
        }
    } else {
        # filter once with all conditions
        if (nrow(iDIF)>0){
            updated <- filter(updated, !(item %in% iDIF$item))
        }
    }

    if (step){
        shift <- shift |>
            mutate(cor_afr = round(cor(updated$delta.x_dev, updated$delta.y_dev), 3),
                   shift_afr = round(mean(updated$delta.x_dev)-mean(updated$delta.y_dev), 3),
                   sdr_afr = round(sd(updated$delta.y_dev)/sd(updated$delta.x_dev), 3))
    } else {
        shift <- shift |>
            mutate(cor_afr = round(cor(updated$delta.x, updated$delta.y_adj), 3),
                   shift_afr = round(mean(updated$delta.x)-mean(updated$delta.y), 3),
                   sdr_afr = round(sd(updated$delta.y)/sd(updated$delta.x), 3))
    }

    # plot non-DIF anchors
    p2 <- plot_DIF(error_band(updated), 'After', vars, p_cut, DIF_cut,
             DIF_adj_cut, step, TRUE, shift$cor_afr, shift$shift_afr,
             shift$sdr_afr, getRange(updated, step), quick)

    p_save <- p1 / p2 +
        plot_annotation(title=paste0('Number of DIF ',
                                     if(step) 'Steps ' else 'Items ', 'in ',
                                     test, ' on ', toupper(DIFVar), ': ',
                                     nrow(results)-nrow(updated)),
                        subtitle=paste0(vars[[1]], ' vs. ', vars[[2]]),
                        tag_levels='I')

    # DIF anchors found
    if('iStep' %in% names(results)) {
        iDIF <- setdiff(results$iStep, updated$iStep)
        results_flag <- mutate(results, flag=ifelse(iStep %in% iDIF, 1, NA))
    } else {
        iDIF <- setdiff(results$item, updated$item)
        results_flag <- mutate(results, flag=ifelse(item %in% iDIF, 1, NA))
    }

    # list to save and return
    N_facil <- mutate(N_facil, item=results_flag$item)

    flag <- right_join(N_facil, results_flag, by='item')
    names(flag) <- gsub("\\.x", str_c('_', vars[[1]]), names(flag))
    names(flag) <- gsub("\\.y", str_c('_', vars[[2]]), names(flag))

    final <- right_join(N_facil, updated, by='item')
    names(final) <- gsub("\\.x", str_c('_', vars[[1]]), names(final))
    names(final) <- gsub("\\.y", str_c('_', vars[[2]]), names(final))

    results_ls <- list(
        comments = DIF_comment_dich(DIFVar, iDIF),
        step = if (step) DIF_steps_dich_step(iterative) else DIF_steps_dich(iterative),
        shift = shift,
        flag = flag,
        final = final
    )

    # DIF: facility
    if(!step){
        sht_facil <- paste0(DIFVar, if(step) '_step', '_', test, '_Facility')
        p_DIF_facil <- plot_facil(test=test, vars=vars, facil_cut=facil_cut,
                                  facil_dif=facil_DIF(test, DIFVar),
                                  DIFVar=DIFVar)
    }

    # save results and plots
    output <- append(results_ls, list(plot_DIF=p_save))
    if (!step) output <- append(output, list(plot_facil=p_DIF_facil))

    if (save_xlsx){
        sht <- paste0(DIFVar, if(step) '_step', '_', test)
        writexl::write_xlsx(results_ls, paste0('DIF/', sht, '.xlsx'))
        ggsave(paste0('DIF/', sht, '.pdf'), p_save, width=17, height=30, units="cm")
        ggsave(paste0('DIF/', sht_facil, '.pdf'), p_DIF_facil, width=20, height=20, units="cm")

        if (step){
            rmd_file <- system.file("rmd", "DIF_dich_step.Rmd", package = "RaschKit")
            rmarkdown::render(
                rmd_file,
                params=list(
                  output=output, test=test,
                  DIFVar=DIFVar, vars=vars,
                  facil_cut=facil_cut
                ),
                output_file=str_c(DIFVar, '_', test, '_step.html'),
                output_dir = 'DIF',
                quiet=TRUE
            )

        } else {
            rmd_file <- system.file("rmd", "DIF_dich.Rmd", package = "RaschKit")
            rmarkdown::render(
                rmd_file,
                params = list(
                    output = output, test=test,
                    DIFVar = DIFVar, vars=vars,
                    facil_cut = facil_cut
                ),
                output_file = str_c(DIFVar, '_', test, '.html'),
                output_dir = 'DIF',
                quiet = TRUE
            )
        }

        # point users to files of varying purposes
        writeLines(c(
            paste0('\n========= Output Files =========\n'),
            paste0(toupper(DIFVar), ' DIF analysis for ', test,
                   if (step) ' (step)', ' (Chi-squared tests):'),
            paste0('\tSummary:\t', 'DIF/', sht, '.xlsx'),
            paste0('\tDIF Plot:\t', 'DIF/', sht, '.pdf'),
            if (!step){
                paste0('\tFacility plot:\t', 'DIF/', sht_facil, '.pdf')
            },
            paste0('\tDIF report:\t', 'DIF/', DIFVar, '_', test, if (step) '_step', '.html')
        ))
    } else{
        output
    }
}

