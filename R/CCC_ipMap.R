#' CCC_ipMap
#'
#' This function draws for each item CCC of both category and score. Plots will be saved in 'output' folder in the working directory.
#'
#' @param folder Working directory where .thr and .cqs are located. Default is 'output' folder.
#' @param test Name of the test.
#' @param cqs CQS output file from ConQuest.
#' @param abilEst2use Ability type used for curve data. Default is 'pv1'. Use 'wle' for smaller samples.
#' @param numAbilGrps Number of ability groups. Default is NULL.
#' @param long_label Whether item labels are longer than 16 characters' fixed width. Default is FALSE.
#' @param poly_key TRUE if the key of any item has polytomous scoring. Default is FALSE.
#' @return Plots of CCC by category and score.
#' @examples
#' plot_data <- CCC_ipMap(test='AHU', cqs=cqs)
#' plot_data <- CCC_ipMap(test='RACP', abilEst2use='wle')
#' @export

CCC_ipMap <- function(folder=here::here('output'), test, cqs, abilEst2use='pv1',
                       numAbilGrps=NULL, long_label=FALSE, poly_key=FALSE){
    thr <- df_thr(folder=folder, test=test, long_label=long_label)

    # check names
    if (!('i_pvmeansd' %in% names(cqs$gMatrixList))){
        names(cqs$gMatrixList)[which(names(cqs$gMatrixList)=='i_abilitymeansd')] <- 'i_pvmeansd'
    }
    if (!('i_itemtotrestcor' %in% names(cqs$gMatrixList))){
        names(cqs$gMatrixList)[which(names(cqs$gMatrixList)=='i_itemstats')] <- 'i_itemtotrestcor'
    }

    # Keys --------------------------------------------------------------------
    keys <- cqs$gKeys %>% {
      tibble(key = map(., "Key"),
             score = map_chr(., "Score"))
      } %>%
      rowid_to_column("listNum") %>%
      unnest(cols = key) %>%
      mutate(key = map_chr(key, 1)) %>%
      group_by(listNum) %>%
      mutate(iNum = row_number(),
             score = as.numeric(score)) %>%
      ungroup() %>%
      filter(key != "x") %>%
      select(-listNum)

    # Person Stats ------------------------------------------------------------
    pStats <- cqs$gAllCaseEstimates %>% {
        tibble(pid = map_int(., "pid"),
               wle = map_dbl(., "wle"), wleSE = map_dbl(., "wleerr"),
               pvs = map(., "pvs"), # plausible values
               score = map_dbl(., "scores"),
               maxScore = map_dbl(., "maxscores"))
    } %>%
        ####################################
    # temporary fix to determine those with score of zero
    # needs to be revisited often
    mutate(extreme = (wle*2)/2)

    if (all(pStats$extreme==-Inf)){
        pStats <- cqs$gAllCaseEstimates %>% {
            tibble(pid = map_int(., "pid"),
                   wle = map_dbl(., "alla_eap"), wleSE = map_dbl(., "alla_eaperr"),
                   pvs = map(., "pvs"), # plausible values
                   score = map_dbl(., "scores"),
                   maxScore = map_dbl(., "maxscores"))
        } %>%
            ####################################
        # temporary fix to determine those with score of zero
        # needs to be revisited often
        mutate(extreme = (wle*2)/2) %>%
            filter(!extreme %in% c(-Inf)) %>%
            select(-extreme) %>%
            #####################################
        mutate(pvsCount = map_int(pvs, length),
               # to ensure that all are captured if there are > 5 pvs
               pv1 = map_dbl(pvs, 1), pv2 = map_dbl(pvs, 2), pv3 = map_dbl(pvs, 3),
               pv4 = map_dbl(pvs, 4), pv5 = map_dbl(pvs, 5))
    } else {
        pStats <- pStats %>%
            filter(!extreme %in% c(-Inf)) %>%
            select(-extreme) %>%
            #####################################
        mutate(pvsCount = map_int(pvs, length),
               # to ensure that all are captured if there are > 5 pvs
               pv1 = map_dbl(pvs, 1), pv2 = map_dbl(pvs, 2), pv3 = map_dbl(pvs, 3),
               pv4 = map_dbl(pvs, 4), pv5 = map_dbl(pvs, 5))
    }

    # Item Difficulties (iLogit) and deltas -----------------------------------
    iStepsCounts <- tibble(
      temp = cqs$gGinLongLabels %>% unlist() %>% str_remove_all(., "item:|(|)"),
      iStepsCount = cqs$gItemSteps %>% unlist()
    ) %>%
      separate(temp, into = c("iNum", "iLab"), sep = " ") %>%
      mutate(iLab = str_remove_all(iLab, "[(,)]"),
             iNum = as.numeric(iNum),
             iType = case_when(iStepsCount == 2 ~ "Dichotomous",
                               iStepsCount > 2 ~ "Polytomous")) %>%
        filter(!is.na(iType))

    # items excluded from calibration
    iExc <- iStepsCounts %>% filter(iStepsCount == 0) %>%
      select(iNum, iLab)

    iExcCheck <- cqs$gDeletes %>% unlist()
    # an alternative to check if this yields the same items

    iEstTemp <- tibble(Xsi = cqs$gXsi %>% as.vector(),
                       error = c(sqrt(cqs$gQuickErrorsXsi)))%>%
      rowid_to_column() %>%
      left_join(
        tibble(paramLab = cqs$gXsiParameterLabels %>% unlist()) %>%
          rowid_to_column(),
        by = "rowid") %>%
      mutate(paramLab = str_trim(paramLab, "both"),
             paramLab = str_squish(paramLab),
             paramLab = str_remove(paramLab, "item "),
             iLab = str_extract(paramLab, "^[\\w-]+"), # extract the first word
             category = str_remove(paramLab, iLab),
             category = str_sub(category, -1, -1),
             category = as.numeric(category)) %>%
      select(-rowid, -paramLab) %>%
      filter(!is.na(iLab)) %>%
      mutate(modelTerm = ifelse(is.na(category), 1, 2))
      # model term 1 is "item" of the model statement "item + item*step"
      # model term 2 is "item*step"
      # simple RM has no second model term

    # tackle constraint terms
    n_should <- nrow(filter(iStepsCounts, !is.na(iType)))
    n_given <- iEstTemp %>%
        filter(modelTerm == 1) %>%
        nrow()
    if(n_given!=n_should){
        constrained1 <- iStepsCounts %>% filter(iNum == max(iStepsCounts$iNum)) %>%
              select(iLab) %>%
              bind_cols(iEstTemp %>% filter(modelTerm == 1) %>%
                          group_by(modelTerm) %>%
                          summarise(Xsi = sum(Xsi)*-1, error = mean(error)))

        constrained1 <- iEstTemp %>% filter(modelTerm == 1) %>%
          group_by(modelTerm) %>%
          dplyr::summarise(Xsi = sum(Xsi)*-1,
                    # sum of all other items' Xsi values multiplied by -1
                    # making the total of all Xsi values including constrained item equal to zero
                    error = mean(error)) %>%
          mutate(iLab = iStepsCounts[iStepsCounts$iNum == max(iStepsCounts$iNum),]$iLab)
    }else{constrained1 <- tibble()}

    if(2 %in% unique(iEstTemp$modelTerm)){
      constrained2 <- iEstTemp %>%
          filter(modelTerm > 1) %>%
          group_by(iLab, modelTerm) %>%
          dplyr::summarise(Xsi = sum(Xsi)*-1,
                    category = max(category) + 1,
                    error = mean(error))
    }else{constrained2 <- tibble()}

    iEstimates <- iEstTemp %>%
      bind_rows(constrained1, constrained2)
    # rm(iEstTemp, constrained1, constrained2)

    # if test has only dichomtomously scored items, then CQ's item delta is iLogit (item difficulty)
    deltas <- iStepsCounts %>%
      filter(iStepsCount > 0) %>%
      left_join(iEstimates %>%
                filter(modelTerm == 1) %>%
                dplyr::rename(iLogit = Xsi),
                by = "iLab")  %>%
      select(-category, -modelTerm) %>%
      left_join(iEstimates %>% filter(modelTerm == 2) %>%
                dplyr::rename(stepDelta = Xsi, stepError = error),
                by = "iLab")  %>%
      select(-modelTerm) %>%
      mutate(stepDelta = ifelse(is.na(stepDelta) & iStepsCount == 2, 0, stepDelta),
             category = ifelse(is.na(category) & iStepsCount == 2, 1, category),
             delta = iLogit + stepDelta) %>%
      filter(!is.na(iType))

    # if quick error is used: Adjust delta shift to delta and abilities
    delta_shift <- mean(deltas$delta)
    if (abs(delta_shift) > 0.001) {
        deltas$iLogit <- deltas$iLogit - delta_shift
        deltas$delta <- deltas$delta - delta_shift
        pStats$wle <- pStats$wle - delta_shift
        pStats <- modify_at(pStats, c('pv1','pv2','pv3','pv4','pv5'), ~{.x-delta_shift})
    }

    # Response Data -----------------------------------------------------------
    preKeyLookUp <- cqs$gPreKeyLookUp %>% as_tibble() %>%
      rowid_to_column() %>% unnest_longer(col = Value) %>%
      mutate(respPrekey = rowid-1) %>%  # because CQ starts with 0
      select(respPrekey, respCat = Value)

    respDat <- cqs$gResponseData %>% {
      tibble(pid = map_dbl(., "Pid"), resp = map_int(., "Rsp"),
             item = map_int(., "Item"), respPrekey = map_int(., "PreKeyRsp"))
      } %>%
      mutate(iNum = item+1) %>% # because CQ starts with item 0
      left_join(preKeyLookUp, by = "respPrekey") %>%
      right_join(deltas %>% distinct(iNum, iLab, iStepsCount),
                by = "iNum") %>%
      filter(pid %in% pStats$pid)

    # Test/Scale Summary Stats ------------------------------------------------
    testStats <- cqs$gMatrixList$i_summarystats %>% as_tibble() %>%
      gather(Statistic, Value)

    # Item Fit stats ----------------------------------------------------------
    # needs to be rewritten if the fit stats of categories of polytomous items have to be included
    iFitStats <- iStepsCounts %>% filter(iStepsCount > 0) %>%
      # rowid_to_column() %>%
      filter(!is.na(iType)) %>%
      bind_cols(cqs$gFitStatistics$Value %>% {
        tibble(UnWeightedMNSQ = map_dbl(., "UnWeightedMNSQ"),
               UnWeightedtfit = map_dbl(., "UnWeightedtfit"),
               WeightedCW2 = map_dbl(., "WeightedCW2"),
               WeightedMNSQ = map_dbl(., "WeightedMNSQ"),
               Weightedtfit = map_dbl(., "Weightedtfit"),
               WeightedNumerator = map_dbl(., "WeightedNumerator"),
               WeightedDenominator = map_dbl(., "WeightedDenominator"),
               UnWeightedSE = map_dbl(., "UnWeightedSE"),
               WeightedSE = map_dbl(., "WeightedSE"))})

    # Category Stats ----------------------------------------------------------
    catStats <- bind_cols(
      cqs$gMatrixList$i_counts %>% as_tibble(),
      cqs$gMatrixList$i_ptbis %>% as_tibble()
      ) %>%
      rowid_to_column("iNum") %>%
      gather(stat, value, -iNum) %>%
      separate(stat, into = c("stat", "resp"), sep = "_")  %>%
      bind_rows(
        cqs$gMatrixList$i_pvmeansd %>% as_tibble() %>%
          rowid_to_column("iNum") %>%
          gather(stat, value, -iNum) %>%
          mutate(stat = str_sub(stat, 1, -3)) %>%
          separate(stat, into= c("resp", "stat"), sep = "_")
        ) %>%
      spread(stat, value, fill= NA) %>%
      filter(count > 0) %>%
      left_join(keys %>% dplyr::rename(resp = key),
                by = c("iNum", "resp")) %>%
      group_by(iNum) %>%
      mutate(score = ifelse(is.na(score), 0, score),
             cands = sum(count),
             percentTotal = count*100/cands) %>%
      left_join(iStepsCounts %>% select(iNum, iLab, iType),
                by = "iNum") %>%
      mutate(nonkeyPtbisPos = ifelse(score == 0 & ptbis > 0, 1, 0),
             keyPtbisNeg = ifelse(score == 1 & ptbis < 0, 1, 0),
             keyPtbisLow = ifelse(score == 1 & ptbis < .1, 1, 0)) %>%
      filter(!is.na(iType))

    # Item Stats --------------------------------------------------------------
    iStats <- catStats %>%
      mutate(scorexpercent = score*percentTotal) %>%
      dplyr::group_by(iNum) %>%
      dplyr::summarise(Facility = sum(scorexpercent)/max(score))  %>%
      # calculation of facility will reproduce how CQ reports facility of polytomous items
      # if dichomotous, Facility is percentage of score of 1
      rowid_to_column() %>%  # if no item was excluded from calibration, then rowid = iNum
      left_join(
        cqs$gMatrixList$i_itemtotrestcor %>% as_tibble() %>%
          dplyr::rename(`Item-Rest` = `item-rest`, `Item-Total` = `item-total`) %>%
          rowid_to_column("iNum"),
          # the new column is iNum and not rowid because gMatrixList includes the excluded items
        by = "iNum") %>%
      left_join(deltas %>% select(-stepDelta, -stepError) %>%
                  mutate(category = paste0("deltaCat", category)) %>%
                  spread(category, delta, fill = NA),
                by = "iNum") %>%
      left_join(thr %>% mutate(category = paste0("thrCat", category)) %>%
                  spread(category, threshold, fill = NA),
                by = c("iNum", "iLab")) %>%
      left_join(iFitStats, by = c("iNum", "iLab", "iStepsCount", "iType")) %>%
      filter(!is.na(iType))

    ###### Item-Person Map ######
    axisLims <- c(pStats$wle, iStats$deltaCat1) %>%
        range()

    pPlot <- pStats %>%
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
        iPlotDat <- iStats %>%
            # mutate(item = unique(iEstimates$iLab)) %>%
            select(item=iLab, deltaCat1 ) %>%
            mutate(delta2 = plyr::round_any(deltaCat1, .1, round)) %>%
            group_by(delta2) %>%
            mutate(deltaRank = rank(deltaCat1, ties.method = "first"))
    } else {
        iPlotDat <- iEstimates %>%
            filter(is.na(category)) %>%
            mutate(iNum=1:nrow(.)) %>%
            select(iLab, iNum) %>%
            right_join(iEstimates, by = "iLab") %>%
            filter(!is.na(category)) %>%
            mutate(item=str_c(iLab,'.',category)) %>%
            select(item, deltaCat1=Xsi) %>%
            mutate(delta2 = plyr::round_any(deltaCat1, .3, round)) %>%
            group_by(delta2) %>%
            mutate(deltaRank = rank(deltaCat1, ties.method = "first"))
    }

    iPlotDat <- iPlotDat %>%
        left_join(deltas %>% select(iNum, item=iLab))
    if (ni>100 || np-ni>100){
        iPlot <- ggplot(iPlotDat, aes(x = deltaRank, y = delta2, label = iNum))
    } else {
        iPlot <- ggplot(iPlotDat, aes(x = deltaRank, y = delta2, label = item))
    }

    iPlot <- iPlot +
        geom_hline(yintercept = mean(iPlotDat$deltaCat1, na.rm=TRUE),
                   colour = "grey80", linetype = "longdash") +
        geom_text(size = 2.5) +
        labs(title = "Items", x = "", y = "Logits") +
        scale_y_continuous(
            limits = c(floor(axisLims[1])-.25, ceiling(axisLims[2])+.25),
            position = "right") +
        xlim(0.5, max(iPlotDat$deltaRank) + 0.5) +
        ggthemes::theme_tufte() +
        theme(axis.text.x=element_blank(),
              axis.ticks.x = element_blank())

    ipMap <- ggpubr::ggarrange(pPlot, iPlot, ncol = 2, nrow = 1,
                               widths = c(2, 3))

    # save item-person map
    pdf(file=file.path(folder, paste0(test, "_ipMap.pdf")), width = 7, height = 7)
    print(ipMap)
    dev.off()

    # Characteristic Curves ----------------------------------------------
    ccDat <- pStats %>% select(pid, ability = wle) %>%
        mutate(type = "wle") %>%
        bind_rows(
            pStats %>%
                select(pid, pv1:pv5) %>%
                gather(type, ability, -pid)
        ) %>%
        filter(type %in% abilEst2use) %>%
        left_join(respDat, by = "pid") %>%
        split(.$iNum)

    if (is.null(numAbilGrps)) numAbilGrps <- map(ccDat, ~round(log10(nrow(.x))+1))
    ccDat_ls <- map2(ccDat, numAbilGrps, ~mutate(.x, group = ntile(ability, .y)) %>%
                   group_by(group) %>%
                   mutate(abilityGrp = mean(ability)) %>% ungroup() %>%
                   dplyr::rename(score = resp))
    itype_condition <- reduce(ccDat_ls, bind_rows) %>%
         nest_by(iNum) %>%
         mutate(itype=case_when(
             all(data$iStepsCount==1)~'allwrong',
             all(data$iStepsCount>2)~'poly',
             all(data$respCat %in% c(0, 1, 9))~'score',
             all(data$iStepsCount==2)~'dich')
         ) %>%
         select(-data)
    ccDat_df <- reduce(ccDat_ls, bind_rows)

    ################### CCC data for polytomous or 01 ############################
    dfObs <- ccDat_df %>%
        mutate(group = as.factor(group),
               score = as.factor(score)) %>%
        group_by(iNum, group, Ability = abilityGrp, Category = score) %>%
        dplyr::summarise(Count = n()) %>% ungroup() %>%
        complete(Category, nesting(iNum, group, Ability), fill = list(Count = 0)) %>%
        ########################
        # removes scores with count of zero across ability groups
        group_by(iNum, Category) %>% mutate(scoreCount = sum(Count)) %>%
        filter(scoreCount > 0) %>% select(-scoreCount) %>%
        ########################
        group_by(iNum, Ability, group) %>%
        mutate(prop = Count/sum(Count)) %>%
        ungroup() %>%
        arrange(iNum, group, Category)

    #################### CCC data for distractors ###########################
    if (poly_key){
        ccDat_df <- ccDat_df %>%
            filter(respPrekey>0, respPrekey<9)
    }
    dfObs_opt <- ccDat_df %>%
        mutate(respCat=toupper(respCat),
               respCat=ifelse(score==1, str_c(respCat, '*'), respCat),
               group = as.factor(group),
               score = as.factor(score)) %>%
        group_by(iNum, group, Ability = abilityGrp, Option = respCat) %>%
        dplyr::summarise(Count = n()) %>% ungroup() %>%
        complete(Option, nesting(iNum, group, Ability), fill = list(Count = 0)) %>%
        ########################
        # removes scores with count of zero across ability groups
        group_by(iNum, Option) %>% mutate(scoreCount = sum(Count)) %>%
        filter(scoreCount > 0) %>% select(-scoreCount) %>%
        ########################
        group_by(iNum, Ability, group) %>%
        mutate(prop = Count/sum(Count)) %>%
        ungroup() %>%
        arrange(iNum, group)

    dfModel <- deltas %>% distinct(iNum, iLab, category, iLogit, delta, stepDelta) %>%
        left_join(thr,  by = c("iNum", "iLab", "category")) %>%
        # mutate(stepThr = ifelse(stepDelta == 0, 0, iLogit + threshold)) %>%
        arrange(iNum, category) %>%
        group_by(iNum) %>%
        mutate(stepDeltaCumsum = cumsum(stepDelta))

    ccc_dich <- function(iStats, dfObs_opt, i){
        i_df <- iStats %>% filter(iNum == i)
        o_df <- dfObs_opt %>% filter(iNum == i)
        xMin <- c(o_df$Ability) %>% min()  %>% floor() -.25
        m_df <- tibble(pLoc = seq(xMin, 3, .001)) %>%
            mutate(pSuccess = exp(pLoc - i_df$iLogit)/(1+exp(pLoc - i_df$iLogit)),
                   lineLab = "Model\nProbability")
        ggplot() +
            geom_point(data = o_df,
                       mapping = aes(x = Ability, y = prop, size = Count,
                                     colour = Option), alpha = .8) +
            geom_line(data = o_df,
                      mapping = aes(x = Ability, y = prop, colour = Option),
                      size = 1, alpha = .5) +
            geom_line(data = m_df,
                      mapping = aes(x = pLoc, y = pSuccess,
                                    colour = lineLab), size = 1.5, alpha = .5) +
            scale_shape_discrete(name = "Ability\nGroup") +
            labs(title = paste0("Category Characteristic Curves\n", "Item: ", i, " (",
                                i_df$iLab,")"),
                 subtitle = paste0("Weighted MNSQ = ", round(i_df$WeightedMNSQ,2)),
                 x = "Latent Trait (logit)", y = "Probability") +
            scale_colour_brewer(type="qual", palette = 2) +
            ggthemes::theme_tufte()
    }

    ccc_poly <- function(iStats, dfObs, i, dfModel){
        i_df <- iStats %>% filter(iNum == i)
        o_df <- dfObs %>% filter(iNum == i) %>%
            mutate(lineLab = "Observed\nProportion")
        # xMin <- min(o_df$Ability) %>% floor() - 0.25
        xMin <- -15
        xmax <- 15

        kkk <- tibble(iNum = i, ability = seq(xMin, xmax, by=.02)) %>%
            left_join(dfModel %>% filter(iNum == i) %>%
                          select(iNum, category, iLogit, stepDeltaCumsum),
                      by = "iNum") %>%
            group_by(iNum, ability) %>%
            arrange(iNum, ability, category) %>%
            mutate(numerator = ((category*(ability - iLogit)) -
                                    stepDeltaCumsum) %>% exp(),
                   denominator = 1+ sum(numerator))
        m_df <- kkk %>%
            select(iNum, ability, category, numerator, denominator) %>%
            ungroup() %>%
            bind_rows(kkk %>% distinct(iNum, ability, denominator) %>%
                          mutate(category = 0, numerator = 1)) %>%
            mutate(probability = numerator/denominator,
                   Category = as.factor(category),
                   lineLab = "Model\nProbability")
        ggplot() +
            geom_point(o_df, mapping = aes(x = Ability, y = prop, size = Count,
                                           colour = Category),  alpha = .8) +
            geom_line(o_df, mapping = aes(x = Ability, y = prop,colour = Category,
                                          linetype = lineLab),
                      size = 1, alpha = .8) +
            geom_line(m_df, mapping= aes(x=ability, y=probability,
                                         colour=Category, linetype = lineLab)) +
            labs(title = paste0("Category Characteristic Curves\n", "Item: ", i, " (",
                                i_df$iLab,")"),
                 subtitle = paste0("Weighted MNSQ = ", round(i_df$WeightedMNSQ,2),
                                   "\nDelta(s) ",
                                   paste(i_df %>% select(contains("delta")) %>%
                                             round(., 2), collapse = " ")),
                 x = "Latent Trait (logit)", y = "Probability",
                 linetype = "") +
            ggthemes::theme_tufte()
    }

    ccc_score <- function(iStats, dfObs, i){
        i_df <- iStats %>% filter(iNum == i)
        o_df <- dfObs %>% filter(iNum == i)
        xMin <- c(o_df$Ability) %>% min()  %>% floor() -.25
        m_df <- tibble(pLoc = seq(xMin, 3, .001)) %>%
            mutate(pSuccess = exp(pLoc - i_df$iLogit)/(1+exp(pLoc - i_df$iLogit)),
                   lineLab = "Model\nProbability")
        ggplot() +
            geom_point(data = o_df,
                       mapping = aes(x = Ability, y = prop, size = Count,
                                     colour = Category), alpha = .8) +
            geom_line(data = o_df,
                      mapping = aes(x = Ability, y = prop, colour = Category),
                      size = 1, alpha = .5) +
            geom_line(data = m_df,
                      mapping = aes(x = pLoc, y = pSuccess,
                                    colour = lineLab), size = 1.5, alpha = .5) +
            scale_shape_discrete(name = "Ability\nGroup") +
            labs(title = paste0("Category Characteristic Curves\n", "Item: ", i, " (",
                                i_df$iLab,")"),
                 subtitle = paste0("Weighted MNSQ = ", round(i_df$WeightedMNSQ,2)),
                 x = "Latent Trait (logit)", y = "Probability") +
            scale_colour_brewer(type="qual", palette = 2) +
            ggthemes::theme_tufte()
    }

    plot_ls <- list()
    ccc_data_ls <- list()
    for(i in seq_along(iStats$iNum)){
        j <- iStats$iNum[[i]]
        # print(i)
        if (itype_condition$itype[[i]] %in% c('allwrong', 'dich')){
            plot_ls[[i]] <- ccc_dich(iStats=iStats, dfObs_opt=dfObs_opt, i=j)
            ccc_data_ls[[i]] <- dfObs_opt %>% filter(iNum == j)
        } else if (itype_condition$itype[[i]] %in% 'poly'){
            plot_ls[[i]] <- ccc_poly(iStats=iStats, dfObs=dfObs, i=j, dfModel=dfModel)
        } else if (itype_condition$itype[[i]] %in% 'score'){
            plot_ls[[i]] <- ccc_score(iStats=iStats, dfObs=dfObs, i=j)
            ccc_data_ls[[i]] <- dfObs %>% filter(iNum == j) %>%
                mutate(Category=ifelse(Category==1, str_c(1, '*'), 0)) %>%
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
    pdf(file=file.path(folder, paste0(test, "_CCC.pdf")), width = 10, height = 7)
    map(plot_ls, ~print(.x))
    dev.off()

    list(itype=itype_condition,
         ccc_data=ccc_data,
         plots=plot_ls,
         ipMap=ipMap)
}
