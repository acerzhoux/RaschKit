#' check_convergence
#'
#' This function checks convergence of the model by checking model deviance
#' change and maximum changes in estimates of delta's, covariance, and regressors.
#' This is associated with test named 'test'. A plot of the checks is saved in
#' 'output' folder in the working directory.
#'
#' @param cqs CQS output file from ConQuest.
#' @param test Test name.
#' @examples
#' check_convergence(test='AHU', cqs=cqs)
#' @export

check_convergence <- function(cqs, test){
  # lookup tbl: Item
  lookup <- tibble(
    Parameters=cqs$gXsiParameterLabels |>
      unlist() |>
      str_remove('item ') |>
      str_remove( ' category ') |>
      str_trim(),
    stat=str_c('Xsi', 1:length(Parameters))
  )

  # estimation history
  cq_hist <- as_tibble(getCqHist(cqs)) |>
    pivot_longer(cols=-Iter, names_to="stat", values_to="Estimate") |>
    mutate(Parameter=ifelse(str_detect(stat, "Xsi"), "Xsi", stat)) |>
    arrange(stat, Iter) |>
    group_by(stat) |>
    rename(Iteration=Iter)

  # Beta
  p1 <- ggplot(
    data=cq_hist |>
      filter(Parameter=='Xsi') |>
      left_join(lookup, by='stat'),
    aes(x=Iteration, y=Estimate, color=Parameters, group=Parameters)
  ) +
    scale_x_continuous(label=scales::label_comma(accuracy=1))
  if (nrow(lookup) > 100) {
    p1 <- p1 + geom_line(show.legend = FALSE)
  } else {
    p1 <- p1 + geom_line()
  }
  pBeta <- p1 +
    ggthemes::theme_tufte() +
    labs(title='Delta')

  # other parameters
  catAll <- setdiff(
    unique((cq_hist$Parameter)),
    c('Xsi','RunNo', "RanTermVariance1", "Tau1")
  )
  plotCats <- c('Beta', 'Likelihood', 'Variance', 'Covariance')
  plotOthers <- list()
  for (i in seq_along(plotCats)){
    plotGrp <- str_subset(catAll, plotCats[[i]])
    plotOthers[[plotCats[[i]]]] <- ggplot(
      data=cq_hist |> filter(Parameter %in% plotGrp),
      aes(x=Iteration, y=Estimate, group=Parameter, color=Parameter)
    ) +
      geom_line() +
      ggthemes::theme_tufte() +
      labs(title=plotCats[[i]]) +
      scale_x_continuous(label=scales::label_comma(accuracy=1))
  }

  plot_ls <- list(
    pBeta,
    patchwork::wrap_plots(plotOthers, ncol=floor(sqrt(length(plotOthers))))
  )

  pdf(file=paste0('output/', test, "_Convergence_check.pdf"), width = 12, height = 6)
  map(plot_ls, ~print(.x))
  dev.off()
}

