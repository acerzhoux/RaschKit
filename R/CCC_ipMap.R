#' CCC_ipMap
#'
#' This function draws for each item CCC of both category and score.
#' Plots will be saved in 'output' folder.
#'
#' @param test Name of the test.
#' @param cqs CQS output file from ConQuest.
#' @param abilEst2use Ability type used for curve data. Default is 'pv1'.
#' Use 'wle' for smaller samples.
#' @param numAbilGrps Number of ability groups. Default is NULL.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @param quick TRUE if empirical error is not needed. Default is TRUE
#' @return Plots of CCC by category and score.
#' @examples
#' plot_data <- CCC_ipMap(test, cqs)
#' plot_data <- CCC_ipMap(test='RACP', abilEst2use='wle')
#' @export

CCC_ipMap <- function(test, cqs, abilEst2use='pv1', numAbilGrps=NULL,
            poly_key=FALSE, quick=TRUE){
  thr <- df_thr('output', test)

  # check names
  if (!('i_pvmeansd' %in% names(cqs$gMatrixList))){
    names(cqs$gMatrixList)[which(names(cqs$gMatrixList)=='i_abilitymeansd')] <- 'i_pvmeansd'
  }
  if (!('i_itemtotrestcor' %in% names(cqs$gMatrixList))){
    names(cqs$gMatrixList)[which(names(cqs$gMatrixList)=='i_itemstats')] <- 'i_itemtotrestcor'
  }

  # Keys --------------------------------------------------------------------
  keys <- tibble(
      key = map(cqs$gKeys, "Key"),
      score = map_chr(cqs$gKeys, "Score")
    ) |>
    rowid_to_column("listNum") |>
    unnest(cols = key) |>
    mutate(key = map_chr(key, 1)) |>
    group_by(listNum) |>
    mutate(
      iNum = row_number(),
      score = as.numeric(score)
    ) |>
    ungroup() |>
    filter(key != "x") |>
    select(-listNum)

  # Person Stats ------------------------------------------------------------
  pStats <- tibble(
      pid = map_int(cqs$gAllCaseEstimates, "pid"),
      wle = map_dbl(cqs$gAllCaseEstimates, "wle"),
      wleSE = map_dbl(cqs$gAllCaseEstimates, "wleerr"),
      pvs = map(cqs$gAllCaseEstimates, "pvs"), # plausible values
      score = map_dbl(cqs$gAllCaseEstimates, "scores"),
      maxScore = map_dbl(cqs$gAllCaseEstimates, "maxscores")
    ) |>
    mutate(extreme = (wle*2)/2) |> # temporary fix: Begin; determine cases of score 0
    filter(!extreme %in% c(-Inf)) |>
    select(-extreme) |> # temporary fix: End
    mutate(
      pvsCount = map_int(pvs, length),
      # to ensure that all are captured if there are > 5 pvs
      pv1 = map_dbl(pvs, 1),
      pv2 = map_dbl(pvs, 2),
      pv3 = map_dbl(pvs, 3),
      pv4 = map_dbl(pvs, 4),
      pv5 = map_dbl(pvs, 5)
    )

  # Item Difficulties (iLogit) and deltas -----------------------------------
  iStepsCounts <- tibble(
      temp = cqs$gGinLongLabels |> unlist() |> str_remove_all("item:|(|)"),
      iStepsCount = cqs$gItemSteps |> unlist()
    ) |>
    separate(temp, into = c("iNum", "iLab"), sep = " ") |>
    mutate(
      iLab = str_remove_all(iLab, "[(,)]"),
      # iLab = keyDf$Item,
      iNum = as.numeric(iNum),
      iType = case_when(
        iStepsCount == 2 ~ "Dichotomous",
        iStepsCount > 2 ~ "Polytomous"
      )
    ) |>
    filter(!is.na(iType))

  # items excluded from calibration
  iExc <- iStepsCounts |> filter(iStepsCount == 0) |>
    select(iNum, iLab)

  iExcCheck <- cqs$gDeletes |> unlist()
  # an alternative to check if this yields the same items

  iEstTemp <- tibble(
      Xsi = cqs$gXsi |> as.vector(),
      error = c(sqrt(cqs$gQuickErrorsXsi))
    ) |>
    rowid_to_column() |>
    left_join(
      tibble(
        paramLab = cqs$gXsiParameterLabels |> unlist()
      ) |>
        rowid_to_column(),
      by = "rowid") |>
    mutate(
      paramLab = str_trim(paramLab, "both"),
      paramLab = str_squish(paramLab),
      paramLab = str_remove(paramLab, "item "),
      iLab = str_extract(paramLab, "^[\\w-]+"), # extract the first word
      category = str_remove(paramLab, iLab),
      category = str_sub(category, -1, -1),
      category = as.numeric(category)
    ) |>
    select(-rowid, -paramLab) |>
    filter(!is.na(iLab)) |>
    mutate(modelTerm = ifelse(is.na(category), 1, 2)) # 'item'; 'step'

  # tackle constraint terms
  # n_should <- nrow(filter(iStepsCounts, !is.na(iType)))
  if(!quick){
    constrained1 <- iStepsCounts |>
      filter(iNum == max(iStepsCounts$iNum)) |>
      select(iLab) |>
      bind_cols(
        iEstTemp |>
          filter(modelTerm == 1) |>
          group_by(modelTerm) |>
          summarise(Xsi = sum(Xsi)*-1, error = mean(error))
      )

    constrained1 <- iEstTemp |>
      filter(modelTerm == 1) |>
      group_by(modelTerm) |>
      dplyr::summarise(
        Xsi = sum(Xsi)*-1, # constraint
        error = mean(error)
      ) |>
      mutate(iLab = iStepsCounts[iStepsCounts$iNum == max(iStepsCounts$iNum),]$iLab)
  } else {
    constrained1 <- tibble()
  }

  if(2 %in% unique(iEstTemp$modelTerm)){
    constrained2 <- iEstTemp |>
      filter(modelTerm > 1) |>
      group_by(iLab, modelTerm) |>
      dplyr::summarise(
        Xsi = sum(Xsi)*-1,
        category = max(category) + 1,
        error = mean(error)
      )
  } else {
    constrained2 <- tibble()
  }

  iEstimates <- iEstTemp |>
    bind_rows(constrained1, constrained2)

  # if test has only dichomtomously scored items, then CQ's item delta is iLogit (item difficulty)
  deltas <- iStepsCounts |>
    filter(iStepsCount > 0) |>
    left_join(iEstimates |>
            filter(modelTerm == 1) |>
            dplyr::rename(iLogit = Xsi),
          by = "iLab") |>
    select(-category, -modelTerm) |>
    left_join(
      iEstimates |>
        filter(modelTerm == 2) |>
        dplyr::rename(stepDelta = Xsi, stepError = error),
      by = "iLab"
    ) |>
    select(-modelTerm) |>
    mutate(
      stepDelta = ifelse(is.na(stepDelta) & iStepsCount == 2, 0, stepDelta),
      category = ifelse(is.na(category) & iStepsCount == 2, 1, category),
      delta = iLogit + stepDelta
    ) |>
    filter(!is.na(iType))

  # if quick error is used: Adjust delta shift to delta and abilities
  delta_shift <- mean(deltas$delta, na.rm=TRUE)
  if (abs(delta_shift) > 0.001) {
    deltas$iLogit <- deltas$iLogit - delta_shift
    deltas$delta <- deltas$delta - delta_shift
    pStats$wle <- pStats$wle - delta_shift
    pStats <- modify_at(pStats, c('pv1','pv2','pv3','pv4','pv5'), ~{.x-delta_shift})
  }

  # Response Data -----------------------------------------------------------
  preKeyLookUp <- cqs$gPreKeyLookUp |> as_tibble() |>
    rowid_to_column() |> unnest_longer(col = Value) |>
    mutate(respPrekey = rowid-1) |>  # because CQ starts with 0
    select(respPrekey, respCat = Value)

  respDat <- tibble(
    pid = map_dbl(cqs$gResponseData, "Pid"),
    resp = map_int(cqs$gResponseData, "Rsp"),
    item = map_int(cqs$gResponseData, "Item"),
    respPrekey = map_int(cqs$gResponseData, "PreKeyRsp")
  ) |>
    mutate(iNum = item+1) |> # because CQ starts with item 0
    left_join(preKeyLookUp, by = "respPrekey") |>
    right_join(
      deltas |> distinct(iNum, iLab, iStepsCount),
      by = "iNum"
    ) |>
    filter(pid %in% pStats$pid)

  # Test/Scale Summary Stats ------------------------------------------------
  testStats <- cqs$gMatrixList$i_summarystats |>
    as_tibble() |>
    gather(Statistic, Value)

  # Item Fit stats ----------------------------------------------------------
  iFitStats <- iStepsCounts |>
    filter(iStepsCount > 0) |>
    filter(!is.na(iType)) |>
    left_join(
      cbind(
        iNum=filter(deltas, !is.na(iLogit)) |>
          pull(iNum),
        tibble(
          UnWeightedMNSQ = map_dbl(cqs$gFitStatistics$Value, "UnWeightedMNSQ"),
          UnWeightedtfit = map_dbl(cqs$gFitStatistics$Value, "UnWeightedtfit"),
          WeightedCW2 = map_dbl(cqs$gFitStatistics$Value, "WeightedCW2"),
          WeightedMNSQ = map_dbl(cqs$gFitStatistics$Value, "WeightedMNSQ"),
          Weightedtfit = map_dbl(cqs$gFitStatistics$Value, "Weightedtfit"),
          WeightedNumerator = map_dbl(cqs$gFitStatistics$Value, "WeightedNumerator"),
          WeightedDenominator = map_dbl(cqs$gFitStatistics$Value, "WeightedDenominator"),
          UnWeightedSE = map_dbl(cqs$gFitStatistics$Value, "UnWeightedSE"),
          WeightedSE = map_dbl(cqs$gFitStatistics$Value, "WeightedSE")
        )[1:nrow(filter(iEstTemp, modelTerm == 1)), ] # polytomous
      ),
      by='iNum'
    )

  # Category Stats ----------------------------------------------------------
  catStats <- bind_cols(
    cqs$gMatrixList$i_counts |> as_tibble(),
    cqs$gMatrixList$i_ptbis |> as_tibble()
  ) |>
    rowid_to_column("iNum") |>
    gather(stat, value, -iNum) |>
    separate(stat, into = c("stat", "resp"), sep = "_") |>
    bind_rows(
      cqs$gMatrixList$i_pvmeansd |>
        as_tibble() |>
        rowid_to_column("iNum") |>
        gather(stat, value, -iNum) |>
        mutate(stat = str_sub(stat, 1, -3)) |>
        separate(stat, into= c("resp", "stat"), sep = "_")
    ) |>
    spread(stat, value, fill= NA) |>
    filter(count > 0) |>
    left_join(
      keys |> dplyr::rename(resp = key),
      by = c("iNum", "resp")
    ) |>
    group_by(iNum) |>
    mutate(
      score = ifelse(is.na(score), 0, score),
      cands = sum(count),
      percentTotal = count*100/cands
    ) |>
    left_join(
      iStepsCounts |>
        select(iNum, iLab, iType),
      by = "iNum"
    ) |>
    mutate(
      nonkeyPtbisPos = ifelse(score == 0 & ptbis > 0, 1, 0),
      keyPtbisNeg = ifelse(score == 1 & ptbis < 0, 1, 0),
      keyPtbisLow = ifelse(score == 1 & ptbis < .1, 1, 0)
    ) |>
    filter(!is.na(iType)) |>
    modify_if(is.numeric, ~round(.x, 2))

  # Item Stats --------------------------------------------------------------
  iStats <- catStats |>
    mutate(scorexpercent = score*percentTotal) |>
    dplyr::group_by(iNum) |>
    dplyr::summarise(Facility = sum(scorexpercent)/max(score)) |>
    rowid_to_column() |>  # if no item was excluded from calibration, then rowid = iNum
    left_join(
      cqs$gMatrixList$i_itemtotrestcor |>
        as_tibble() |>
        dplyr::rename(
          `Item-Rest` = `item-rest`,
          `Item-Total` = `item-total`
        ) |>
        rowid_to_column("iNum"),
      # the new column is iNum and not rowid because gMatrixList includes the excluded items
      by = "iNum"
    ) |>
    left_join(
      deltas |>
        select(-stepDelta, -stepError) |>
        mutate(category = paste0("deltaCat", category)) |>
        unique() |>
        spread(category, delta, fill = NA),
      by = "iNum"
    ) |>
    left_join(
      thr |>
        mutate(category = paste0("thrCat", category)) |>
        spread(category, threshold, fill = NA),
      by = c("iNum", "iLab")
    ) |>
    left_join(
      iFitStats,
      by = c("iNum", "iLab", "iStepsCount", "iType")
    ) |>
    filter(!is.na(iType))

  ###### Item-Person Map ######
  axisLims <- c(pStats$wle, iStats$deltaCat1) |>
    range()

  pPlot <- pStats |>
    ggplot(aes(x = wle)) +
    xlim(floor(axisLims[1])-.25, ceiling(axisLims[2])+.25) +
    geom_histogram(binwidth = .1) +
    coord_flip() +
    scale_y_reverse() +
    labs(title = "Candidates", x = "", y = "") +
    ggthemes::theme_tufte() +
    theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

  np <- nrow(iEstimates)
  ni <- length(which(iEstimates$modelTerm==1))
  if ( np==ni || np-ni < ni){
    iPlotDat <- iStats |>
      # mutate(item = unique(iEstimates$iLab)) |>
      select(item=iLab, deltaCat1 ) |>
      mutate(delta2 = plyr::round_any(deltaCat1, .15, round)) |>
      group_by(delta2) |>
      mutate(deltaRank = rank(deltaCat1, ties.method = "first"))
  } else {
    iPlotDat <- iEstimates |>
      filter(is.na(category)) |>
      rowid_to_column('iNum') |>
      # mutate(iNum=1:nrow(.)) |>
      select(iLab, iNum) |>
      right_join(iEstimates, by = "iLab") |>
      filter(!is.na(category)) |>
      mutate(item=str_c(iLab,'.',category)) |>
      select(item, deltaCat1=Xsi) |>
      mutate(delta2 = plyr::round_any(deltaCat1, .15, round)) |>
      group_by(delta2) |>
      mutate(deltaRank = rank(deltaCat1, ties.method = "first"))
  }

  iPlotDat <- iPlotDat |>
    left_join(deltas |> select(iNum, item=iLab), by = "item")
  if (ni>100 || np-ni>100){
    iPlot <- ggplot(iPlotDat, aes(x = deltaRank, y = delta2, label = iNum))
  } else {
    iPlot <- ggplot(iPlotDat, aes(x = deltaRank, y = delta2, label = item))
  }

  iPlot <- iPlot +
    geom_hline(yintercept = mean(iPlotDat$deltaCat1, na.rm=TRUE),
           colour = "grey80", linetype = "longdash") +
    geom_text(size = 2) +
    labs(title = "Items", x = "", y = "Logits") +
    scale_y_continuous(
      limits = c(floor(axisLims[1])-.25, ceiling(axisLims[2])+.25),
      position = "right") +
    xlim(0.5, max(iPlotDat$deltaRank) + 0.5) +
    ggthemes::theme_tufte() +
    theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

  ipMap <- ggpubr::ggarrange(pPlot, iPlot, ncol = 2, nrow = 1,
                 widths = c(1, 4))

  # save item-person map
  pdf(file = paste0('output/', test, "_ipMap.pdf"), width = 7, height = 7)
  print(ipMap)
  dev.off()

  # Characteristic Curves ----------------------------------------------
  ccDat <- pStats |>
    select(pid, ability = wle) |>
    mutate(type = "wle") |>
    bind_rows(
      pStats |>
        select(pid, pv1:pv5) |>
        gather(type, ability, -pid)
    ) |>
    filter(type %in% abilEst2use) |>
    left_join(respDat, by = "pid")
  ccDat <- split(ccDat, ccDat$iNum)

  if (is.null(numAbilGrps)) numAbilGrps <- map(ccDat, ~round(log10(nrow(.x))+1))
  ccDat_ls <- map2(ccDat, numAbilGrps, ~mutate(.x, group = ntile(ability, .y)) |>
             group_by(group) |>
             mutate(abilityGrp = mean(ability)) |> ungroup() |>
             dplyr::rename(score = resp))
  itype_condition <- reduce(ccDat_ls, bind_rows) |>
    nest_by(iNum) |>
    mutate(itype=case_when(
      all(data$iStepsCount==1)~'allwrong',
      all(data$iStepsCount>2)~'poly',
      all(data$respCat %in% c(0,1,9,'M','R','m','r'))~'score',
      all(data$iStepsCount==2)~'dich')
    ) |>
    select(-data)
  ccDat_df <- reduce(ccDat_ls, bind_rows)

  ################### CCC data for polytomous or 01 ############################
  dfObs <- ccDat_df |>
    mutate(
      group = as.factor(group),
      score = as.factor(score)
    ) |>
    group_by(iNum, group, Ability = abilityGrp, Category = score) |>
    dplyr::summarise(Count = n()) |> ungroup() |>
    complete(Category, nesting(iNum, group, Ability), fill = list(Count = 0)) |>
    ########################
  # removes scores with count of zero across ability groups
  group_by(iNum, Category) |> mutate(scoreCount = sum(Count)) |>
    filter(scoreCount > 0) |> select(-scoreCount) |>
    ########################
  group_by(iNum, Ability, group) |>
    mutate(prop = Count/sum(Count)) |>
    ungroup() |>
    arrange(iNum, group, Category)

  #################### CCC data for distractors ###########################
  if (poly_key){
    ccDat_df <- ccDat_df |>
      filter(respPrekey>0, respPrekey<9)
  }

  dfObs_opt <- ccDat_df |>
    mutate(respCat=toupper(respCat),
         respCat=ifelse(score==1, str_c(respCat, '*'), respCat),
         group = as.factor(group),
         score = as.factor(score)) |>
    group_by(iNum, group, Ability = abilityGrp, Option = respCat) |>
    dplyr::summarise(Count = n()) |> ungroup() |>
    complete(Option, nesting(iNum, group, Ability), fill = list(Count = 0)) |>
    ########################
  # removes scores with count of zero across ability groups
  group_by(iNum, Option) |> mutate(scoreCount = sum(Count)) |>
    filter(scoreCount > 0) |> select(-scoreCount) |>
    ########################
  group_by(iNum, Ability, group) |>
    mutate(prop = Count/sum(Count)) |>
    ungroup() |>
    arrange(iNum, group)

  dfModel <- deltas |> distinct(iNum, iLab, category, iLogit, delta, stepDelta) |>
    left_join(thr,  by = c("iNum", "iLab", "category")) |>
    # mutate(stepThr = ifelse(stepDelta == 0, 0, iLogit + threshold)) |>
    arrange(iNum, category) |>
    group_by(iNum) |>
    mutate(stepDeltaCumsum = cumsum(stepDelta))

  # tbl put below CCC
  tblInt <- catStats |>
    ungroup() |>
    select(
      iNum,
      Option=resp, Score=score, Count=count,
      `% Total`=percentTotal,
      `Pt Bis`=ptbis, PV1Avg=mean, PV1SD=sd
    )

  plot_ls <- list()
  ccc_data_ls <- list()
  for(i in seq_along(iStats$iNum)){
    # for(i in 5){
    j <- iStats$iNum[[i]]
    # print(i)
    if (itype_condition$itype[[i]] %in% c('allwrong', 'dich')){
      plot_ls[[i]] <- CCC_plot()[['ccc_dich']](iStats, dfObs_opt, j, tblInt)
      ccc_data_ls[[i]] <- dfObs_opt |> filter(iNum == j)
    } else if (itype_condition$itype[[i]] %in% 'poly'){
      plot_ls[[i]] <- CCC_plot()[['ccc_poly']](iStats, dfObs, j, dfModel, tblInt)
    } else if (itype_condition$itype[[i]] %in% 'score'){
      plot_ls[[i]] <- CCC_plot()[['ccc_score']](iStats, dfObs, j, tblInt)
      ccc_data_ls[[i]] <- dfObs |> filter(iNum == j) |>
        mutate(Category=ifelse(Category==1, str_c(1, '*'), 0)) |>
        dplyr::rename(Option=Category)
    }
  }

  # tackle all-polytomous-item case
  if (all(itype_condition$itype=='poly')){
    ccc_data <- tibble(iNum=1:length(plot_ls), group=NA)
  } else {
    ccc_data <- reduce(ccc_data_ls, bind_rows)
  }

  # ####### generate ICCs by Score
  pdf(file = paste0('output/', test, "_CCC.pdf"), width = 7, height = 9)
  map(plot_ls, ~print(.x))
  dev.off()

  list(
    itype=itype_condition,
    ccc_data=ccc_data,
    plots=plot_ls,
    ipMap=ipMap
  )
}
