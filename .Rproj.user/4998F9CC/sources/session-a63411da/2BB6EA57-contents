#' DIF_VarTests
#'
#' This function performs DIF analysis on variables and responses in the data.
#' Data should be processed to have DIF variables of only categories needed.
#' This is associated with test named 'test'. An Excel file with a summary of
#' the DIF analysis results and plots will be saved in 'DIF' folder in the
#' working directory.
#'
#' @param testVec Vector of test names. Default is NULL.
#' @param respDfLst Dataframe of DIF variables and responses.
#' @param difVarLst List of DIF variables. Name is variable name. Element is name
#' of categories which corresponds to data coding order. Polytomous DIF variable
#' should have element as NULL. Default is NULL.
#' @param pid Name of candidates' ID variable. Default is NULL.
#' @param n_cov Number of covariates before responses. Default is NULL.
#' @param keyDfLst List of dataframe of 'Item', 'Key', and 'Max_score'
#' (add Key2 if double key). Default is NULL.
#' @param p_cut p value of chi-square test. Default is 0.05.
#' @param DIF_cut Threshold of an item's delta estimate difference between two
#' testVec. Default is 0.5.
#' @param DIF_adj_cut Threshold of an item's adjusted delta estimate difference
#' between two testVec. Default is 4.
#' @param desig_effect Value to adjust errors. Default is 1.
#' @param step TRUE if DIF analysis is performed on step parameters. Default
#' is FALSE.
#' @param resltReady TRUE if results are ready. Default is FALSE.
#' @param iter TRUE to iteratively remove DIF items. Default is TRUE.
#' @param test3term Vector of names of testVec to include a three-way interaction
#' (item, step, DIF variable). Default is NULL.
#' @examples
#' DIF_VarTests()
#' @export

DIF_VarTests <- function(testVec=NULL, respDfLst=NULL, difVarLst=NULL, n_cov=NULL, pid=NULL,
                         keyDfLst=NULL, p_cut=0.05, DIF_cut=0.5, DIF_adj_cut=4,
                         desig_effect=1, step=FALSE, resltReady=FALSE,
                         iter=TRUE, test3term=NULL){
  # check input
  if (is.null(testVec) | is.null(difVarLst)){
    stop('Please provide \'testVec\' and \'difVarLst\'!')
  }
  if (!is.null(test3term)){
    if (!(all(test3term %in% testVec))){
      stop('testVec in \'test3term\' should be one of \'testVec\'!')
    }
  }
  if (!resltReady) {
    if (is.null(respDfLst) | is.null(n_cov) | is.null(pid) | is.null(keyDfLst)){
      stop('Please provide \'respDfLst\', \'n_cov\', \'pid\', and \'keyDfLst\'!')
    }
    if (!all(names(difVarLst) %in% names(respDfLst[[1]]))) {
      stop('All DIF variables should be in data!')
    }
    if (!(length(difVarLst) == length(keyDfLst))) {
      stop('difVarLst and keyDfLst should have same number of elements!')
    }
  }

  n_var <- length(difVarLst)
  vars_DIF <- names(difVarLst)

  if (resltReady) { # extract stats and DIF analysis
    for (k in seq_along(testVec)){
      test <- testVec[[k]]
      for (i in 1:n_var){
        DIFVar <- vars_DIF[[i]]
        vars <- difVarLst[[i]]
        if (is.null(vars)) {
          DIF_poly_shw(DIFVar, test, NULL, p_cut, FALSE, NULL)
        } else {
          DIF_dich_its_shw(DIFVar, test, vars, p_cut, DIF_cut, DIF_adj_cut,
                           desig_effect, FALSE, TRUE, iter, TRUE)
        }
      }
      if (!is.null(test3term)){
        if (test %in% test3term){
          DIF_dich_its_shw(DIFVar, test, vars, p_cut, DIF_cut, DIF_adj_cut,
                           desig_effect, TRUE, TRUE, iter, TRUE)
        }
      }
    }
  } else { # model, save results, and DIF analysis

    for (k in seq_along(testVec)){
      test <- testVec[[k]]
      keys <- keyDfLst[[k]]
      if (is.null(respDfLst)){
        data <- respDfLst
      } else {
        data <- respDfLst[[k]]
      }
      for (i in 1:n_var){
        DIFVar <- vars_DIF[[i]]
        vars <- difVarLst[[i]]
        method <- ifelse(is.null(vars), 'Bonferroni', 'chi_square')
        DIF_dim_one(method, test, pid, n_cov, DIFVar, data, 'sav', keys, vars,
                    FALSE, NULL, NULL, TRUE, NULL, FALSE, TRUE, p_cut, DIF_cut,
                    DIF_adj_cut, iter, FALSE, desig_effect, NULL)
      }
      if (!is.null(test3term)){
        if (test %in% test3term){
          DIF_dim_one(method, test, pid, n_cov, DIFVar, data, 'sav', keys, vars,
                      FALSE, NULL, NULL, TRUE, NULL, FALSE, TRUE, p_cut, DIF_cut,
                      DIF_adj_cut, iter, TRUE, desig_effect, NULL)
        }
      }
    }
  }

  # summarise results; add format
  for (i in 1:n_var){
    DIFVar <- vars_DIF[[i]]
    folder <- paste0('DIF/', DIFVar)
    files <- paste0(folder, '/', testVec, '_process.xlsx')
    vars <- difVarLst[[i]]

    if (is.null(vars)){
      ex_ls <- map(files, ~readxl::read_xlsx(.x, 'final')) |>
        `names<-`(testVec) |>
        imap(~mutate(.x, Test=.y) |> select(Test, everything()))

      summary <- map(
        ex_ls,
        ~mutate(.x, flag=apply(.x, 1, function(x) any(x %in% '*'))) |>
          filter(flag) |> select(-flag)
        ) |>
        reduce(bind_rows) |>
        arrange(Test, items)

      # add favor, disfavor flags
      nCat <- (ncol(summary)-2)/2
      catVec <- unique(str_extract(names(summary)[3:ncol(summary)], '\\d+'))
      summary$Favored <- summary$Disfavored <- NA
      for (j in 0:(nCat-1)){
        nCol <- 2*j+3
        for (i in 1:nrow(summary)){
        if (!is.na(summary[i, nCol+1])){
          if (summary[i, nCol] > 0) {
            summary[i, 'Disfavored'] <- paste(
              na.omit(c(summary[[i, 'Disfavored']], catVec[j+1])),
              collapse = ', '
            )
          } else {
            summary[i, 'Favored'] <- paste(
              na.omit(c(summary[[i, 'Favored']], catVec[j+1])),
              collapse = ', '
            )
          }
        }
        }
      }

      ls_save <- list(summary=summary) |>
        append(ex_ls)

      # add format
      file <- paste0(folder, ".xlsx")
      add_format()[['DIFPoly']](
        ls_save,
        folder,
        file
      )
      cat('\n', DIFVar, 'DIF analysis summary file is at:\n\t', file)

    } else { # dichotomous DIFVar; step parameters
      if (!is.null(test3term)){
        testsT <- testVec[which(testVec %in% test3term)]
        test3termFiles <- paste0(folder, '/step_', testsT, '_process.xlsx')
        files <- c(files, test3termFiles)
        testVec <- c(testVec, paste0('step_', testsT))
      }

      ex_ls <- map(files, ~readxl::read_xlsx(.x, 'final'))
      names(ex_ls) <- testVec

      n <- length(ex_ls)
      nStep <- ifelse(is.null(test3term), 0, length(test3termFiles))

      summary <- map(ex_ls[1:(n-nStep)], ~.x |> filter(flag==1)) |>
        imap(~.x |> mutate(Test=.y)) |>
        map2(
          difVarLst,
          ~.x |> mutate(Favored=as.character(ifelse(DIF<0, .y[[1]], .y[[2]])))
        ) |>
        reduce(bind_rows) |>
        select(Test, Favored, everything(), chisq, p) |>
        arrange(Test, Favored, item)

      if (!is.null(test3term)){
        summaryStep <- map(ex_ls[(n-nStep+1):n], ~.x |> filter(flag==1)) |>
          imap(~.x |> mutate(Test=.y)) |>
          map2(
            difVarLst,
            ~.x |> mutate(Favored=as.character(ifelse(DIF<0, .y[[1]], .y[[2]])))
          ) |>
          reduce(bind_rows) |>
          select(Test, Favored, everything(), chisq, p) |>
          arrange(Test, Favored, item)

        n1 <- nrow(summary)
        n2 <- nrow(summaryStep)
        if (n1==0) {
          summary <- summaryStep
        } else if (n2==0) {
          summary <- summary
        } else {
          check <- n1-n2
          if (check < 0) {
            summary[(n1+1):n2, ] <- NA
          } else if (check > 0) {
            summaryStep[(n2+1):n1, ] <- NA
          }
          summary <- bind_cols(summary, ` `=NA) |>
            cbind(summaryStep)
        }
      }

      ls_save <- list(summary=summary) |>
        append(ex_ls)

      # add format
      file <- paste0(folder, ".xlsx")
      add_format()[['equate']](
        ls_save,
        folder,
        file,
        c(DIF_cut, DIF_adj_cut)
      )
      cat('\n', DIFVar, 'DIF analysis summary file is at:\n\t', file)
    }
  }

}
