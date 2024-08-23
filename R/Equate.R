#' Equate
#'
#' This function performs chi-square tests (DIF analysis) on a group of anchor
#' items' difference of delta estimates in two tests. An Excel file with test
#' results and flags can be saved in 'equating' folder. Also, one plot is saved
#' in subfolder 'plot' inside 'equating' folder.
#'
#' @param deltaDf Dataframe of five variables of 'item' (anchor labels), 'delta.x',
#' 'error.x', 'delta.y', and 'error.y'. '.x' and '.y' should correspond to order
#' of test names in 'vars'.
#' @param test Name of test such as 'AGAT'.
#' @param vars Vector of length 2 such as c('VIC','NSW'). Its order corresponds
#' to two tests associated with .x and .y.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' tests. Default is 0.5.
#' @param DIF_std_cut Threshold of an item's standardized delta estimate difference
#' between two tests. Default is 4.
#' @param sav_results TRUE if an Excel file with chi-square test results and
#' a plot are desired. Default is TRUE.
#' @param design_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters.
#' Default is FALSE.
#' @param DIFVar Name of dichotomous DIF variable, e.g., 'gender'.
#' @param iter TRUE to iteratively remove DIF items. Default is FALSE.
#' @param indDf Dataframe of indice which includes variables of 'item' and
#' 1-3 of facility, discrimination, and weighted fit in order. Default is NULL when
#' none of those indice is available.
#' @param pointer Whether to print locations of output files. Default is FALSE.
#' @param report Whether to generate a html report. Default is FALSE.
#' @param sigma Indicator of how 'delta.y' is scaled. If TRUE, it is scaled
#' to have same mean and sd as 'delta.x'. If FALSE, it has same mean as 'delta.x'.
#' Default is FALSE.
#' @return Dataframe of chi-square test results for anchors between two tests.
#' @examples
#' Equate(deltaDf=data[, c('item', 'delta.x', 'error.x', 'delta.y', 'error.y')],
#' test='Elana_math', vars=c('NSW', 'VIC'))
#' @export

Equate <- function(deltaDf, test, vars, p_cut=0.05, DIF_cut=0.5, DIF_std_cut=4,
                   sav_results=TRUE, design_effect=1, step=FALSE, DIFVar=NULL,
                   iter=FALSE, indDf=NULL, pointer=FALSE, report=FALSE, sigma=FALSE){
  # check
  if (!is.null(indDf)){
    if (nrow(deltaDf) != nrow(indDf)) {
      stop('deltaDf and indDf should have same number of rows!')
    }
    if (!('item' %in% names(indDf))) {
      stop('indDf should have variable \'item\' as item labels!')
    }
    indDf <- modify_if(indDf, is.numeric, ~round(.x, 3))
  }
  if (!('item' %in% names(deltaDf))) {
    stop('deltaDf should have variable \'item\' as item labels!')
  }
  deltaDf <- modify_if(deltaDf, is.numeric, ~round(.x, 3))

  # set up folders
  if (is.null(DIFVar)) {
    entry <- 'equating'
    if (!dir.exists(entry)) dir.create(entry)
    folder <- ifelse(
      step,
      paste0(entry, '/step_', vars[[1]], ' vs ', vars[[2]]),
      paste0(entry, '/', vars[[1]], ' vs ', vars[[2]])
    )
  } else {
    entry <- 'DIF'
    if (!dir.exists(entry)) dir.create(entry)
    folder <- paste0(entry, '/', DIFVar)
  }

  if (sav_results) {
    if (!dir.exists(folder)) dir.create(folder)
  }

  prefix <- ifelse(
    step,
    paste0(folder, '/step_', test, '_'),
    paste0(folder, '/', test, '_')
  )

  # function to get range of x, y axis
  getRange <-  function(data, step){
    if (step){
      range(data[['delta.x_dev']], data[['delta.y_dev']]) + c(-.3, .3)
    } else {
      range(data[['delta.x']], data[['delta.y_adj']]) + c(-.3, .3)
    }
  }

  # DIF check
  if (step){
    results <- chisqTStep(deltaDf, design_effect)
  } else {
    results <- chisqT(deltaDf, sigma)
  }

  iDIF <- DIF_items(results, p_cut, DIF_cut, DIF_std_cut)
  iDIFFlag <- pull(iDIF, item)

  updated <- results
  flag <- mutate(results, flag=ifelse(item %in% iDIFFlag, 1, 0))

  # two ways of dealing with DIF items
  if (iter){
    iFlag <- list()
    # iteraively remove DIF anchor of max chisq
    while (dim(iDIF)[1] != 0){
      iFlag <- iFlag |>
        append(
          list(filter(iDIF, chisq==max(chisq)))
        )
      iItem <- filter(iDIF, chisq==max(chisq)) |>
        pull(item)
      if (step){
        updated <- chisqTStep(filter(updated, item!=iItem), design_effect)
      } else {
        updated <- chisqT(filter(updated, item!=iItem))
      }
      iDIF <- DIF_items(updated, p_cut, DIF_cut, DIF_std_cut)
    }
    if (length(iFlag)>0){
      iFlagDF <- reduce(iFlag, bind_rows)
    } else {
      iFlagDF <- NULL
    }
  } else {
    # filter once with all conditions
    if (nrow(iDIF)!=0){
      updated <- results |> filter(!(item %in% iDIF$item))
      iFlagDF <- results |> filter(item %in% iDIF$item)
    } else {
      iFlagDF <- NULL
    }
  }
  if (is.null(iFlagDF)){
    final <- updated |> mutate(flag=0)
  } else {
    final <- bind_rows(
      iFlagDF |> mutate(flag=1),
      updated |> mutate(flag=0)
    )
  }
  if (!is.null(indDf)){
    flag <- left_join(
        indDf,
        flag,
        by='item'
      )
      # arrange(desc(chisq))

    final <- left_join(
        indDf,
        final,
        by='item'
      )
      # arrange(desc(chisq))
  }

  # plot final: Before removal
  shift <- tibble(cor_bfr = round(cor(final$delta.x, final$delta.y), 3),
                  shift_bfr = round(mean(final$delta.x)-mean(final$delta.y), 3),
                  sdr_bfr = round(sd(final$delta.y)/sd(final$delta.x), 3))

  p1 <- plot_DIF(error_band(final), 'Before', vars, p_cut, DIF_cut,
                 DIF_std_cut, step, DIFVar, shift$cor_bfr, shift$shift_bfr,
                 shift$sdr_bfr, getRange(final, step), quick)

  # plot updated: After removal
  shift <- shift |>
    mutate(cor_afr = round(cor(updated$delta.x, updated$delta.y), 3),
         shift_afr = round(mean(updated$delta.x)-mean(updated$delta.y), 3),
         sdr_afr = round(sd(updated$delta.y)/sd(updated$delta.x), 3))

  # plot non-DIF anchors
  p2 <- plot_DIF(error_band(updated), 'After', vars, p_cut, DIF_cut,
                 DIF_std_cut, step, DIFVar, shift$cor_afr, shift$shift_afr,
                 shift$sdr_afr, getRange(updated, step), quick)

  p_save <- p1 / p2 +
    plot_annotation(title=paste0('Number of DIF ', if(step) 'Step ' else 'Items ', 'in ',
                   toupper(test), ': ', nrow(deltaDf)-nrow(updated)),
            subtitle=paste0(vars[[1]], ' vs. ', vars[[2]]),
            tag_levels='I')

  iDIF <- setdiff(deltaDf$item, updated$item)

  # change .x, .y in names
  names(flag) <- gsub("\\.x", str_c(' ', vars[[1]]), names(flag))
  names(flag) <- gsub("\\.y", str_c(' ', vars[[2]]), names(flag))
  names(flag) <- gsub("_", ' ', names(flag))

  names(final) <- gsub("\\.x", str_c(' ', vars[[1]]), names(final))
  names(final) <- gsub("\\.y", str_c(' ', vars[[2]]), names(final))
  names(final) <- gsub("_", ' ', names(final))

  output <- list(
    comments=DIF_comment_dich_equate(vars=vars, iDIF=iDIF, DIFVar=DIFVar),
    step=if (step) DIF_steps_dich_step(iter=iter) else DIF_steps_dich(iter=iter),
    shift=shift,
    flag=flag,
    final=final,
    plot_DIF=p_save
  )

  if (sav_results) {# save results and plots
    writexl::write_xlsx(output[1:5], paste0(prefix, 'process.xlsx'))
    ggsave(
      paste0(prefix, 'delta.png'),
      p_save,
      width=17, height=30, units="cm"
    )
    if (!is.null(indDf)){
      numInd <- ifelse(!is.null(DIFVar), 2, (ncol(indDf)-1)/2)
      if (!is.null(DIFVar)) {
        colsInd1 <- c(3, 6)
      } else {
        colsInd1 <- c(2, 2+numInd)
      }
      ggsave(
        paste0(prefix, 'facilDiscrFitw.png'),
        plot_facilDiscrFitw(indDf, vars, colsInd1, numInd),
        width=17, height=30, units="cm"
      )
    }

    if (report){
      rmd_file <- system.file("rmd", "Equating_dich.Rmd", package = "RaschKit")
      rmarkdown::render(
        rmd_file,
        params = list(test=test, vars=vars, step=step, DIF=!is.null(DIFVar), output=output),
        output_file = paste0(prefix, 'report.html'),
        output_dir = file.path(getwd(), folder),
        quiet = TRUE
      )
    }

    # point users to files of varying purposes
    if (pointer){
      writeLines(c(
        paste0('\n========= Output Files =========\n'),
        paste0(
          'Anchor DIF analysis for ', test,
          ' (', vars[[2]], ' vs. ', vars[[1]], '):'
        ),
        if (report) paste0('\tReport:\t\t', prefix, 'report.html'),
        paste0('\tSummary:\t', prefix, 'process.xlsx'),
        paste0('\tDelta plot:\t', paste0(prefix, 'delta.png')),
        if (!is.null(indDf)){
          paste0('\tIndice plot:\t', prefix, 'facilDiscrFitw.png')
        }
      ))
    }

  } else {
    output
  }
}
