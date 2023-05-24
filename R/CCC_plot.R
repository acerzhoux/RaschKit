#' CCC_plot
#'
#' This function returns a list of functions that plot CCC for dichotomous,
#' polytomous, and right-wrong items.
#'
#' @param easy Threshold to flag easy items. Default is 90 (percent correct).
#' @param hard Threshold to flag hard items. Default is 10 (percent correct).
#' @param iRst Threshold to flag low item-rest correlation statistics.
#' Default is 0.11.
#' @param fit_w Threshold to flag large weighted item fit statistics.
#' Default is 1.1.
#' @param fit_uw Threshold to flag large unweighted item fit statistics.
#' Default is 1.2.
#' @export

CCC_plot <- function(easy=90, hard=10, iRst=.11, fit_w=1.1, fit_uw=1.2){
  testStatPlot <- function(i, iStats, poly=FALSE) {
    if (poly) {
      i_df <- iStats |>
        filter(iNum == i) |>
        dplyr::select(
          Delta=iLogit,
          contains('delta'),
          Facility,
          Infit=WeightedMNSQ,
          Outfit=UnWeightedMNSQ,
          iRestCor=`Item-Rest`,
          iTotalCor=`Item-Total`
        ) |>
        modify_if(is.numeric, ~round(.x, 2))
    } else {
      i_df <- iStats |>
        filter(iNum == i) |>
        dplyr::select(
          Delta=iLogit,
          Facility,
          Infit=WeightedMNSQ,
          Outfit=UnWeightedMNSQ,
          iRestCor=`Item-Rest`,
          iTotalCor=`Item-Total`
        ) |>
        modify_if(is.numeric, ~round(.x, 2))
    }

    row_iRestFlag <- with(i_df, iRestCor<iRst)
    row_infitFlag <- with(i_df, Infit>fit_w)
    row_outfitFlag <- with(i_df, Outfit>fit_uw)
    row_facilFlag <- with(i_df, Facility>easy | Facility<hard)

    testFex <- i_df |>
      flextable::flextable() |>
      flextable::color(i = row_facilFlag, j = "Facility", color = "red") |>
      flextable::color(i = row_iRestFlag, j = "iRestCor", color = "red") |>
      flextable::color(i = row_infitFlag, j = "Infit", color = "red") |>
      flextable::color(i = row_outfitFlag, j = "Outfit", color = "red") |>
      flextable::bold(i = row_facilFlag, j = "Facility", bold = T) |>
      flextable::bold(i = row_iRestFlag, j = 'iRestCor', bold = T) |>
      flextable::bold(i = row_infitFlag, j = 'Infit', bold = T) |>
      flextable::bold(i = row_outfitFlag, j = 'Outfit', bold = T) |>
      flextable::as_raster()

    ggplot() +
      ggthemes::theme_tufte() +
      annotation_custom(
        grid::rasterGrob(testFex),
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      )
  }

  # flag option: Dichotomous scoring
  optStatPlot <- function(i, tblInt) {
    tblStats <- tblInt |>
      dplyr::filter(iNum==i) |>
      dplyr::select(-iNum) |>
      mutate(Option=ifelse(Score==1, paste0(Option, '*'), Option))

    tblStats <- tblStats |>
      dplyr::mutate(
        key_ptbis = min(tblStats[str_detect(tblStats$Option, "\\*"), ]$`Pt Bis`),
        key_abil = min(tblStats[str_detect(tblStats$Option, "\\*"), ]$PV1Avg), #db key
        unexp_ptbis = ifelse(`% Total`>10 & `Pt Bis`>0 & Score==0, 1, 0),
        unexp_abil = ifelse(`% Total`>10 & PV1Avg>key_abil & Score==0, 1, 0)
      )

    row_key <- with(tblStats, str_detect(Option, "\\*"))
    row_ptbisFlag <- with(tblStats, unexp_ptbis == 1)
    row_abilFlag <- with(tblStats, unexp_abil == 1)

    tblStatsFlex <- tblStats |>
      dplyr::select(Option:PV1SD) |>
      flextable::flextable() |>
      flextable::bg(i = row_key, bg = "yellow", part = "body") |>
      flextable::color(i = row_ptbisFlag, j = "Pt Bis", color = "red") |>
      flextable::color(i = row_abilFlag, j = "PV1Avg", color = "red") |>
      flextable::bold(i = row_ptbisFlag, j = "Pt Bis", bold = T) |>
      flextable::bold(i = row_abilFlag, j = "PV1Avg", bold = T) |>
      flextable::as_raster()

    ggTbl <- ggplot() +
      ggthemes::theme_tufte() +
      annotation_custom(
        grid::rasterGrob(tblStatsFlex),
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      )
  }

  # flag scores: Polytomous scoring
  scoreStatPlot <- function(i, tblInt) {
    tblStats <- tblInt |>
      dplyr::filter(iNum==i) |>
      dplyr::select(-iNum) |>
      mutate(Option=ifelse( Score>0, paste0(Option, '*'), Option)) |>
      rename(Code=Option)

    tblStats <- tblStats |>
      dplyr::mutate(
        flagScore = ifelse((`% Total`<10 & Score>0) | (`% Total`<10 & Code==0),1, 0),
        flagPtBis = ifelse(`Pt Bis`-lag(`Pt Bis`)<0 & Score>0, 1, 0),
        flagPv1 =  ifelse(PV1Avg-lag(PV1Avg)<0 & Score>0, 1, 0)
      )

    row_key <- with(tblStats, str_detect(Code, "\\*"))
    row_ScoreFlag <- with(tblStats, flagScore == 1)
    row_ptbisFlag <- with(tblStats, flagPtBis == 1)
    row_abilFlag <- with(tblStats, flagPv1 == 1)

    tblStatsFlex <- tblStats |>
      dplyr::select(Code:PV1SD) |>
      flextable::flextable() |>
      flextable::bg(i = row_key, bg = "yellow", part = "body") |>
      flextable::color(i = row_ScoreFlag, j = "% Total", color = "red") |>
      flextable::color(i = row_ptbisFlag, j = "Pt Bis", color = "red") |>
      flextable::color(i = row_abilFlag, j = "PV1Avg", color = "red") |>
      flextable::bold(i = row_ScoreFlag, j = "% Total", bold = T) |>
      flextable::bold(i = row_ptbisFlag, j = "Pt Bis", bold = T) |>
      flextable::bold(i = row_abilFlag, j = "PV1Avg", bold = T) |>
      flextable::as_raster()

    ggTbl <- ggplot() +
      ggthemes::theme_tufte() +
      annotation_custom(
        grid::rasterGrob(tblStatsFlex),
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf
      )
  }

  # https://community.appliedepi.org/t/combining-a-ggplot-and-table-in-r/390/2
  ccc_dich <- function(iStats, dfObs_opt, i, tblInt){
    # CCC plot
    i_df <- iStats |>
      filter(iNum == i) |>
      modify_if(is.numeric, ~round(.x, 2))
    o_df <- dfObs_opt |> filter(iNum == i)

    # detect sparse options
    opt_sparse <- o_df |>
      filter(!(Option %in% c('M', 'R'))) |>
      group_by(Option) |>
      summarise(N=sum(Count)) |>
      filter(N < 25) |>
      pull(Option)
    if (length(opt_sparse) > 0){
      opt_text <- str_c(
        'Warning:\nFewer than 25 candidates selected ',
        paste(opt_sparse, collapse=' or '),
        '.'
      )
      annt <- data.frame(
        xpos = -Inf,
        ypos = Inf,
        txt = opt_text,
        hjustvar = 0,
        vjustvar = 2
      )
    }

    xMin <- c(o_df$Ability) |> min() |> floor() -.1
    xMax <- c(o_df$Ability) |> max() |> ceiling() +.1
    m_df <- tibble(pLoc = seq(xMin, xMax, .001)) |>
      mutate(pSuccess = exp(pLoc - i_df$iLogit)/(1+exp(pLoc - i_df$iLogit)),
      lineLab = "Model\nProbability")

    CCC <- ggplot() +
      geom_point(
        data = o_df,
        mapping = aes(x = Ability, y = prop, size = Count, colour = Option),
        alpha = .8
      ) +
      geom_line(
        data = o_df,
        mapping = aes(x = Ability, y = prop, colour = Option),
        size = 1,
        alpha = .5
      ) +
      geom_line(
        data = m_df,
        mapping = aes(x = pLoc, y = pSuccess, colour = lineLab),
        size = 1.5,
        alpha = .5
      ) +
      scale_shape_discrete(name = "Ability\nGroup") +
      labs(
        title = paste0(
          "Category Characteristic Curves\n",
          "Item: ", i, " (", i_df$iLab,")"
        ),
        x = "Latent Trait (logit)",
        y = "Probability"
      ) +
      scale_colour_brewer(type="qual", palette = 2) +
      ggthemes::theme_tufte()

    if (length(opt_sparse) > 0){
      CCC <- CCC +
        geom_text(
          data = annt,
          aes(x=xpos, y=ypos, hjust=hjustvar, vjust=vjustvar, label=txt)
        )
    }

    CCC / optStatPlot(i, tblInt) / testStatPlot(i, iStats) +
      patchwork::plot_layout(heights = c(5, 2, 0.7))
  }

  ccc_poly <- function(iStats, dfObs, i, dfModel, tblInt){
    i_df <- iStats |>
      filter(iNum == i) |>
      modify_if(is.numeric, ~round(.x, 2))
    o_df <- dfObs |>
      filter(iNum == i) |>
      mutate(lineLab = "Observed\nProportion")
    xMin <- -15
    xmax <- 15

    kkk <- tibble(
        iNum = i,
        ability = seq(xMin, xmax, by=.02)
      ) |>
      left_join(
        dfModel |>
          filter(iNum == i) |>
          dplyr::select(
            iNum, category,
            iLogit, stepDeltaCumsum
          ),
        by = "iNum"
      ) |>
      group_by(iNum, ability) |>
      arrange(iNum, ability, category) |>
      mutate(
        numerator = ((category*(ability - iLogit)) - stepDeltaCumsum) |> exp(),
        denominator = 1+ sum(numerator)
      )

    m_df <- kkk |>
      dplyr::select(iNum, ability, category, numerator, denominator) |>
      ungroup() |>
      bind_rows(
        kkk |>
          distinct(iNum, ability, denominator) |>
          mutate(category = 0, numerator = 1)
      ) |>
      mutate(
        probability = numerator/denominator, Category = as.factor(category),
        lineLab = "Model\nProbability"
      )

    CCC <- ggplot() +
      geom_point(
        o_df,
        mapping = aes(x = Ability, y = prop, size = Count, colour = Category),
        alpha = .8
      ) +
      geom_line(
        o_df,
        mapping = aes(x = Ability, y = prop, colour = Category, linetype = lineLab),
        size = 1,
        alpha = .8
      ) +
      geom_line(
        m_df,
        mapping= aes(x=ability, y=probability, colour=Category, linetype = lineLab)
      ) +
      labs(
         title = paste0(
           "Category Characteristic Curves\n",
           "Item: ", i, " (", i_df$iLab,")"
         ),
         x = "Latent Trait (logit)",
         y = "Probability",
         linetype = ""
      ) +
      ggthemes::theme_tufte()

    CCC / scoreStatPlot(i, tblInt) / testStatPlot(i, iStats, TRUE) +
      patchwork::plot_layout(heights = c(5, 2, 0.7))
  }

  ccc_score <- function(iStats, dfObs, i, tblInt){
    i_df <- iStats |>
      dplyr::filter(iNum == i) |>
      modify_if(is.numeric, ~round(.x, 2))
    o_df <- dfObs |>
      filter(iNum == i)
    xMin <- c(o_df$Ability) |> min() |> floor() -.25
    xMax <- c(o_df$Ability) |> max() |> ceiling() +.1

    m_df <- tibble(pLoc = seq(xMin, xMax, .001)) |>
      mutate(
        pSuccess = exp(pLoc - i_df$iLogit)/(1+exp(pLoc - i_df$iLogit)),
        lineLab = "Model\nProbability"
      )

    CCC <- ggplot() +
      geom_point(
        data = o_df,
        mapping = aes(x = Ability, y = prop, size = Count, colour = Category),
        alpha = .8
      ) +
      geom_line(
        data = o_df,
        mapping = aes(x = Ability, y = prop, colour = Category),
        size = 1,
        alpha = .5
      ) +
      geom_line(
        data = m_df,
        mapping = aes(x = pLoc, y = pSuccess, colour = lineLab),
        size = 1.5,
        alpha = .5
      ) +
      scale_shape_discrete(name = "Ability\nGroup") +
      labs(
        title = paste0(
          "Category Characteristic Curves\n",
          "Item: ", i, " (", i_df$iLab,")"
        ),
        x = "Latent Trait (logit)",
        y = "Probability"
      ) +
      scale_colour_brewer(type="qual", palette = 2) +
      ggthemes::theme_tufte()

    CCC / optStatPlot(i, tblInt) / testStatPlot(i, iStats) +
      patchwork::plot_layout(heights = c(5, 2, 0.7))
  }

  # return functions
  list(
    ccc_dich=ccc_dich,
    ccc_poly=ccc_poly,
    ccc_score=ccc_score
  )
}
