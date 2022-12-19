#' plot_DIF_group
#'
#' This function plots expected score curves for each item using results from 
#' item calibration with the DIF variable as a group variable. This is associated 
#' with test named 'test'.
#'
#' @param test Name of test.
#' @param DIFVar Name of DIF variable. Default is NULL.
#' @param abilEst2use Ability type used for curve data. Default is 'pv1'. 
#' For smaller sample size,  'wle' should be considered.
#' @param numAbilGrps Number of ability groups. Default is 3.
#' @examples
#' plot_DIF_group(test='elena', DIFVar='quintile')
#' @export

plot_DIF_group <- function(test, DIFVar, numAbilGrps=3, abilEst2use="pv1"){
    cqs <- conquestr::ConQuestSys(here::here("DIF", DIFVar, paste0(test, '_group.CQS')))

    # Person Stats ------------------------------------------------------------
    pStats <- cqs$gAllCaseEstimates %>% {
        tibble(pid = map_int(., "pid"),
               wle = map_dbl(., "wle"), wleSE = map_dbl(., "wleerr"),
               pvs = map(., "pvs"), # plausible values
               score = map_dbl(., "scores"), maxScore = map_dbl(., "maxscores"))
    } %>%
        ####################################
    # temporary fix to determine those with score of zero
    # needs to be revisited often
    mutate(extr = case_when(wle*2 == -Inf ~ 1,
                            wle*2 == Inf ~ 1,
                            TRUE ~ 0)) %>%
        #####################################
    # filter(pid %in%  c(1:2)) %>%
    mutate(pvsCount = map_int(pvs, length),
           # to ensure that all are captured if there are > 5 pvs
           pv1 = map_dbl(pvs, 1), pv2 = map_dbl(pvs, 2), pv3 = map_dbl(pvs, 3),
           pv4 = map_dbl(pvs, 4), pv5 = map_dbl(pvs, 5))

    # Item Difficulties (iLogit) and deltas -----------------------------------
    iStepsCounts <- tibble(
        temp = cqs$gGinLongLabels %>% unlist() %>% str_remove_all(., "item:|(|)"),
        iStepsCount = cqs$gItemSteps %>% unlist()
    ) %>%
        separate(temp, into = c("iNum", "iLab"), sep = " ") %>%
        mutate(iLab = str_remove_all(iLab, "[(,)]"),
               iNum = as.numeric(iNum),
               iType = case_when(iStepsCount == 2 ~ "Dichotomous",
                                 iStepsCount > 2 ~ "Polytomous"))

    # items excluded from calibration
    # OR items in the item pool but not in the test
    iExc <- iStepsCounts %>% filter(iStepsCount == 0) %>%
        distinct(iNum, iLab)

    # included items
    iInc <- iStepsCounts %>% filter(iStepsCount > 0) %>%
        distinct(iNum, iLab)

    # empirical error
    varCovMat <- cqs$gMatrixList$e_estimatecovariances[(1:nrow(iInc)-1),(1:nrow(iInc)-1)]
    empErrs <- tibble(errorEmp = diag(varCovMat) %>% sqrt()) %>%
        rowid_to_column() %>%
        bind_rows(tibble(rowid = nrow(iInc),
                         errorEmp = sum(varCovMat) %>% sqrt())
        )

    if(is.null(varCovMat) == TRUE){empErrs <- empErrs %>% filter(rowid == 0)}
    iEstTemp <- tibble(Xsi = c(cqs$gXsi),
                       errorQuick = c(sqrt(cqs$gQuickErrorsXsi))) %>%
        rowid_to_column()  %>%
        left_join(tibble(paramLab = cqs$gXsiParameterLabels %>% unlist()) %>%
                      rowid_to_column(),
                  by = "rowid") %>%
        filter(!is.na(paramLab)) %>%
        mutate(paramLab = str_trim(paramLab, "both"),
               paramLab = str_squish(paramLab),
               paramLab = str_remove(paramLab, "item "),
               iLab = str_extract(paramLab, "^[\\w-]+"), # extract the first word
               category = str_remove(paramLab, iLab),
               category = str_sub(category, -1, -1),
               category = as.numeric(category)
        ) %>%
        select(-rowid, -paramLab) %>%
        mutate(modelTerm = ifelse(is.na(category), 1, 2),
               category = ifelse(is.na(category), 1, category)) %>%
        filter(!is.na(iLab))

    constrained1 <- iStepsCounts %>% filter(iNum == max(iStepsCounts$iNum)) %>%
        select(iLab) %>%
        bind_cols(iEstTemp %>% filter(modelTerm == 1) %>%
                      summarise(Xsi = sum(Xsi)*-1, errorQuick = mean(errorQuick), modelTerm = 1)
        )

    if(2 %in% unique(iEstTemp$modelTerm)){
        constrained2 <- iEstTemp %>% filter(modelTerm == 2) %>%
            group_by(iLab) %>%
            summarise(Xsi = sum(Xsi)*-1, errorQuick = mean(errorQuick),
                      category = max(category) + 1) %>%
            ungroup() %>%
            mutate(modelTerm = 2)
    }else{constrained2 <- tibble()}
    ## In shw files, model term 1 refers to the item estimates
    ## and model term 2 refers to item category estimates

    iEstimates <- iEstTemp %>%
        bind_rows(constrained1, constrained2)

    # if test has only dichomtomously scored items, then CQ's item delta is iLogit (item difficulty)
    deltas <- iStepsCounts %>% filter(iStepsCount > 0) %>%
        left_join(iEstimates %>% filter(modelTerm == 1) %>%
                      rename(iLogit = Xsi) %>%
                      select(-category, -modelTerm),
                  by = "iLab"
        )  %>%
        rowid_to_column() %>%
        left_join(empErrs, by = "rowid") %>%
        # empirical errors are used if available
        mutate(error = case_when(is.null(varCovMat) == TRUE ~ errorQuick,
                                 TRUE ~ errorEmp)) %>%
        left_join(iEstimates %>% filter(modelTerm > 1) %>%
                      rename(stepLogit = Xsi) %>%
                      select(-errorQuick, -modelTerm)
        )  %>%
        mutate(stepLogit = ifelse(is.na(stepLogit) & iStepsCount == 2, 0, stepLogit),
               category = ifelse(is.na(category) & iStepsCount == 2, 1, category),
               delta = iLogit + stepLogit)

    # Response Data -----------------------------------------------------------
    preKeyLookUp <- cqs$gPreKeyLookUp %>% tbl_df() %>%
        rowid_to_column() %>% unnest_longer(col = Value) %>%
        mutate(respPrekey = rowid-1) %>%  # because CQ starts with 0
        select(respPrekey, respCat = Value)
    pidVar_groupTab <- cqs$gGroupData %>% {
        tibble(pid = map_chr(., "CaseNum"), var_group = map_chr(., "GData"))
    } %>%
        mutate_all(as.numeric)
    respDat <- cqs$gResponseData %>% {
        tibble(pid = map_dbl(., "Pid"), resp = map_int(., "Rsp"),
               item = map_int(., "Item"), respPrekey = map_int(., "PreKeyRsp"))
    } %>%
        mutate(iNum = item+1) %>% # because CQ starts with item 0
        left_join(preKeyLookUp, by = "respPrekey") %>%
        left_join(deltas %>% distinct(iNum, iLab, iStepsCount),
                  by = "iNum") %>%
        left_join(pidVar_groupTab)

    # Characteristic Curves ----------------------------------------------
    # prepare data
    ccDat <- pStats %>%
        # filter(extr != 1) %>%
        select(pid, ability = wle) %>%
        mutate(type = "wle") %>%
        bind_rows(
            pStats %>%
                # filter(extr != 1) %>%
                select(pid, pv1:pv5) %>%
                gather(type, ability, -pid)
        ) %>%
        group_by(type) %>%
        mutate(group = ntile(ability, numAbilGrps)) %>%
        left_join(respDat, by = "pid") %>% # moved here on 13 March 2020
        group_by(type, var_group, group) %>%
        mutate(abilityGrp = mean(ability)) %>% ungroup() %>%
        rename(score = resp)

    # ICCs by var_group
    dfObserved <- ccDat %>% filter(type == abilEst2use) %>%
        group_by(type, var_group, group) %>%
        mutate(abilityGrp = mean(ability)) %>% ungroup() %>%
        mutate(group = as.factor(group),
               score = as.factor(score),
               # var_group = as.factor(var_group)
        ) %>%
        group_by(iNum, group, Ability = abilityGrp, Score = score, Group = var_group) %>%
        summarise(Count = n()) %>% ungroup() %>%
        complete(Score, nesting(iNum, group, Group, Ability), fill = list(Count = 0)) %>%
        ########################
    # removes scores with count of zero across ability groups
    group_by(iNum, Score) %>% mutate(scoreCount = sum(Count)) %>%
        filter(scoreCount > 0) %>% select(-scoreCount) %>%
        ########################
    group_by(iNum, Ability, Group, group) %>%
        mutate(prop = Count/sum(Count)) %>%
        ungroup() %>%
        arrange(iNum, group, Ability, Score) %>%
        mutate(Group = str_pad(Group, 2, "left", "0"))

    dfObservedAverage <- ccDat %>% filter(type == abilEst2use) %>%
        group_by(type, group) %>%
        mutate(abilityGrp = mean(ability)) %>% ungroup() %>%
        mutate(group = as.factor(group),
               score = as.factor(score)) %>%
        group_by(iNum, group, Ability = abilityGrp, Score = score) %>%
        summarise(Count = n()) %>% ungroup() %>%
        complete(Score, nesting(iNum, group, Ability), fill = list(Count = 0)) %>%
        ########################
    # removes scores with count of zero across ability groups
    group_by(iNum, Score) %>% mutate(scoreCount = sum(Count)) %>%
        filter(scoreCount > 0) %>% select(-scoreCount) %>%
        ########################
    group_by(iNum, Ability, group) %>%
        mutate(prop = Count/sum(Count)) %>%
        ungroup() %>%
        arrange(iNum, group, Ability, Score)

    xMin <- round(min(dfObserved$Ability), 2) - 0.1
    xMax <- round(max(dfObserved$Ability), 2) + 0.1

    pdf(file=here::here('DIF', paste0(DIFVar, '_', test, "_Group.pdf")),
        width = 10, height = 7)

    for(k in unique(deltas$iNum)){
        i_df <- deltas %>% filter(iNum == k)
        o_df <- dfObserved %>% filter(iNum == k, Score == 1) %>%
            bind_rows(dfObservedAverage %>% filter(iNum == k, Score == 1) %>%
                          mutate(Group = "Average\nExpected")) %>%
            mutate(lineLab = "Observed\nProportion")

        m_df <- tibble(iNum = k, ability = seq(xMin, xMax, by=.02)) %>%
            mutate(pSuccess = exp(ability - i_df$delta)/(1+exp(ability - i_df$delta)),
                   lineLab = "Model\nProbability")

        colourCount = length(unique(o_df$Group))
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))

        plot <- ggplot() +
            geom_line(m_df, mapping= aes(x=ability, y=pSuccess, linetype = lineLab), lwd = 1) +
            geom_point(o_df, mapping = aes(x = Ability, y = prop, shape = Group, fill = Group,
                                           colour = Group), size = 4)  +
            geom_line(o_df, mapping = aes(x = Ability, y = prop, colour = Group,
                                          linetype = lineLab),  lwd = .5) +
            scale_colour_manual(values = getPalette(colourCount)) +
            scale_shape_manual(values = 1:colourCount) +
            labs(title = paste0("Expected Score Curve(s)\n", "Item: ", k,
                                " (", i_df$iLab,")"),
                 subtitle = paste("Delta: ", round(i_df$iLogit,2)),
                 x = "Latent Trait (logits)",
                 y = "Probability",
                 linetype = "") +
            scale_x_continuous(breaks=seq(-5, 5, 1)) +
            scale_y_continuous(breaks=seq(.2, 1, .1), limits = c(.2, 1)) +
            ggthemes::theme_tufte()

        print(plot)
    }
    dev.off()
}
