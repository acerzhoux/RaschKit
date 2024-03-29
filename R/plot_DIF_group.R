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
#' plot_DIF_group(test='RANZCOG', DIFVar='Educator')
#' @export

plot_DIF_group <- function(test, DIFVar, numAbilGrps=3, abilEst2use="pv1"){
    cqs <- conquestr::ConQuestSys(paste0("DIF/", DIFVar, '/', test, '_group.CQS'))

    # Person Stats ------------------------------------------------------------
    pStats <-  tibble(
            pid = map_int(cqs$gAllCaseEstimates, "pid"),
            wle = map_dbl(cqs$gAllCaseEstimates, "wle"),
            wleSE = map_dbl(cqs$gAllCaseEstimates, "wleerr"),
            pvs = map(cqs$gAllCaseEstimates, "pvs"), # plausible values
            score = map_dbl(cqs$gAllCaseEstimates, "scores"),
            maxScore = map_dbl(cqs$gAllCaseEstimates, "maxscores")
        ) |>
        ####################################
        # temporary fix to determine those with score of zero
        # needs to be revisited often
        dplyr::mutate(extr = case_when(wle*2 == -Inf ~ 1,
                                wle*2 == Inf ~ 1,
                                TRUE ~ 0)) |>
        #####################################
        # dplyr::filter(pid %in%  c(1:2)) |>
        dplyr::mutate(pvsCount = map_int(pvs, length),
               # to ensure that all are captured if there are > 5 pvs
               pv1 = map_dbl(pvs, 1),
               pv2 = map_dbl(pvs, 2),
               pv3 = map_dbl(pvs, 3),
               pv4 = map_dbl(pvs, 4),
               pv5 = map_dbl(pvs, 5))

        # Item Difficulties (iLogit) and deltas -----------------------------------
        iStepsCounts <- tibble(
            temp = str_remove_all(cqs$gGinLongLabels |> unlist(), "item:|(|)"),
            iStepsCount = cqs$gItemSteps |>
                unlist()
        ) |>
            separate(temp, into = c("iNum", "iLab"), sep = " ") |>
            dplyr::mutate(iLab = str_remove_all(iLab, "[(,)]"),
                   iNum = as.numeric(iNum),
                   iType = case_when(iStepsCount == 2 ~ "Dichotomous",
                                     iStepsCount > 2 ~ "Polytomous"))

    # items excluded from calibration
    # OR items in the item pool but not in the test
    iExc <- iStepsCounts |> dplyr::filter(iStepsCount == 0) |>
        distinct(iNum, iLab)

    # included items
    iInc <- iStepsCounts |> dplyr::filter(iStepsCount > 0) |>
        distinct(iNum, iLab)

    # empirical error
    varCovMat <- cqs$gMatrixList$e_estimatecovariances[(1:nrow(iInc)-1),(1:nrow(iInc)-1)]
    empErrs <- tibble(errorEmp = diag(varCovMat) |> sqrt()) |>
        rowid_to_column() |>
        bind_rows(tibble(rowid = nrow(iInc),
                         errorEmp = sum(varCovMat) |> sqrt())
        )

    if(is.null(varCovMat) == TRUE){empErrs <- empErrs |> dplyr::filter(rowid == 0)}
    iEstTemp <- tibble(Xsi = c(cqs$gXsi),
                       errorQuick = c(sqrt(cqs$gQuickErrorsXsi))) |>
        rowid_to_column()  |>
        left_join(tibble(paramLab = cqs$gXsiParameterLabels |> unlist()) |>
                      rowid_to_column(),
                  by = "rowid") |>
        dplyr::filter(!is.na(paramLab)) |>
        dplyr::mutate(paramLab = str_trim(paramLab, "both"),
               paramLab = str_squish(paramLab),
               paramLab = str_remove(paramLab, "item "),
               iLab = str_extract(paramLab, "^[\\w-]+"), # extract the first word
               category = str_remove(paramLab, iLab),
               category = str_sub(category, -1, -1),
               category = as.numeric(category)
        ) |>
        dplyr::select(-rowid, -paramLab) |>
        dplyr::mutate(modelTerm = ifelse(is.na(category), 1, 2),
               category = ifelse(is.na(category), 1, category)) |>
        dplyr::filter(!is.na(iLab))

    constrained1 <- iStepsCounts |>
        dplyr::filter(iNum == max(iStepsCounts$iNum)) |>
        dplyr::select(iLab) |>
        bind_cols(iEstTemp |>
                      dplyr::filter(modelTerm == 1) |>
                      dplyr::summarise(Xsi = sum(Xsi)*-1,
                                errorQuick = mean(errorQuick),
                                modelTerm = 1)
        )

    if(2 %in% unique(iEstTemp$modelTerm)){
        constrained2 <- iEstTemp |>
            dplyr::filter(modelTerm == 2) |>
            group_by(iLab) |>
            dplyr::summarise(Xsi = sum(Xsi)*-1,
                      errorQuick = mean(errorQuick),
                      category = max(category) + 1) |>
            ungroup() |>
            dplyr::mutate(modelTerm = 2)
    }else{constrained2 <- tibble()}
    ## In shw files, model term 1 refers to the item estimates
    ## and model term 2 refers to item category estimates

    iEstimates <- iEstTemp |>
        bind_rows(constrained1, constrained2)

    # if test has only dichomtomously scored items, then CQ's item delta is iLogit (item difficulty)
    deltas <- iStepsCounts |>
        dplyr::filter(iStepsCount > 0) |>
        left_join(iEstimates |>
                      dplyr::filter(modelTerm == 1) |>
                      dplyr::rename(iLogit = Xsi) |>
                      dplyr::select(-category, -modelTerm),
                  by = "iLab"
        )  |>
        rowid_to_column() |>
        left_join(empErrs, by = "rowid") |>
        # empirical errors are used if available
        dplyr::mutate(error = case_when(is.null(varCovMat) == TRUE ~ errorQuick,
                                 TRUE ~ errorEmp)) |>
        left_join(iEstimates |> dplyr::filter(modelTerm > 1) |>
                      dplyr::rename(stepLogit = Xsi) |>
                      dplyr::select(-errorQuick, -modelTerm),
            by = "iLab"
        )  |>
        dplyr::mutate(stepLogit = ifelse(is.na(stepLogit) & iStepsCount == 2, 0, stepLogit),
               category = ifelse(is.na(category) & iStepsCount == 2, 1, category),
               delta = iLogit + stepLogit)

    # Response Data -----------------------------------------------------------
    preKeyLookUp <- cqs$gPreKeyLookUp |> tibble::as_tibble() |>
        rowid_to_column() |> unnest_longer(col = Value) |>
        dplyr::mutate(respPrekey = rowid-1) |>  # because CQ starts with 0
        dplyr::select(respPrekey, respCat = Value)

    pidVar_groupTab <- tibble(
            pid = as.character(map(cqs$gGroupData, "CaseNum")),
            var_group = map_chr(cqs$gGroupData, "GData")
        ) |>
        dplyr::mutate_all(as.numeric)

    respDat <- tibble(
            pid = map_dbl(cqs$gResponseData, "Pid"),
            resp = map_int(cqs$gResponseData, "Rsp"),
            item = map_int(cqs$gResponseData, "Item"),
            respPrekey = map_int(cqs$gResponseData, "PreKeyRsp")) |>
        dplyr::mutate(iNum = item+1) |> # because CQ starts with item 0
        left_join(preKeyLookUp, by = "respPrekey") |>
        left_join(deltas |> distinct(iNum, iLab, iStepsCount),
                  by = "iNum") |>
        left_join(pidVar_groupTab, by = "pid")

    # Characteristic Curves ----------------------------------------------
    # prepare data
    ccDat <- pStats |>
        dplyr::filter(extr != 1) |>
        dplyr::select(pid, ability = wle) |>
        dplyr::mutate(type = "wle") |>
        bind_rows(
            pStats |>
                dplyr::filter(extr != 1) |>
                dplyr::select(pid, pv1:pv5) |>
                gather(type, ability, -pid)
        ) |>
        group_by(type) |>
        dplyr::mutate(group = ntile(ability, numAbilGrps)) |>
        left_join(respDat, by = "pid") |> # moved here on 13 March 2020
        group_by(type, var_group, group) |>
        dplyr::mutate(abilityGrp = mean(ability)) |> ungroup() |>
        dplyr::rename(score = resp)

    # ICCs by var_group
    dfObserved <- ccDat |>
        dplyr::filter(type == abilEst2use) |>
        group_by(type, var_group, group) |>
        dplyr::mutate(abilityGrp = mean(ability)) |> ungroup() |>
        dplyr::mutate(group = as.factor(group),
               score = as.factor(score),
               # var_group = as.factor(var_group)
        ) |>
        group_by(iNum, group, Ability = abilityGrp, Score = score, !!sym(DIFVar) := var_group) |>
        dplyr::summarise(Count = n()) |> ungroup() |>
        complete(Score, nesting(iNum, group, !!sym(DIFVar), Ability), fill = list(Count = 0)) |>
        ########################
        # removes scores with count of zero across ability groups
        group_by(iNum, Score) |> dplyr::mutate(scoreCount = sum(Count)) |>
            dplyr::filter(scoreCount > 0) |> dplyr::select(-scoreCount) |>
            ########################
        group_by(iNum, Ability, !!sym(DIFVar), group) |>
            dplyr::mutate(prop = Count/sum(Count)) |>
            ungroup() |>
            arrange(iNum, group, Ability, Score) |>
            dplyr::mutate(!!sym(DIFVar) := str_pad(!!sym(DIFVar), 2, "left", "0"))

    dfObservedAverage <- ccDat |>
        dplyr::filter(type == abilEst2use) |>
        group_by(type, group) |>
        dplyr::mutate(abilityGrp = mean(ability)) |> ungroup() |>
        dplyr::mutate(group = as.factor(group),
               score = as.factor(score)) |>
        group_by(iNum, group, Ability = abilityGrp, Score = score) |>
        dplyr::summarise(Count = n()) |> ungroup() |>
        complete(Score, nesting(iNum, group, Ability), fill = list(Count = 0)) |>
        ########################
        # removes scores with count of zero across ability groups
        group_by(iNum, Score) |>
        dplyr::mutate(scoreCount = sum(Count)) |>
            dplyr::filter(scoreCount > 0) |>
        dplyr::select(-scoreCount) |>
            ########################
        group_by(iNum, Ability, group) |>
            dplyr::mutate(prop = Count/sum(Count)) |>
            ungroup() |>
            arrange(iNum, group, Ability, Score)

    xMin <- round(min(dfObserved$Ability), 2) - 0.1
    xMax <- round(max(dfObserved$Ability), 2) + 0.1

    pdf(file = file.path('DIF', DIFVar, paste0(test, '_Group.pdf')), width = 10, height = 7)

    for(k in unique(deltas$iNum)){
        i_df <- deltas |>
            dplyr::filter(iNum == k)
        o_df <- dfObserved |>
            dplyr::filter(iNum == k, Score == 1, !is.na(!!sym(DIFVar))) |>
            bind_rows(dfObservedAverage |>
                          dplyr::filter(iNum == k, Score == 1) |>
                          dplyr::mutate(!!sym(DIFVar) := "Average\nExpected")) |>
            dplyr::mutate(lineLab = "Observed\nProportion")

        m_df <- tibble(iNum = k,
                       ability = seq(xMin, xMax, by=.02)) |>
            dplyr::mutate(pSuccess = exp(ability - i_df$iLogit[[1]])/(1+exp(ability - i_df$iLogit[[1]])),
                   lineLab = "Model\nProbability")

        colourCount = length(unique(o_df[[4]]))
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))

        plot <- ggplot() +
            geom_line(m_df, mapping= aes(x=ability, y=pSuccess, linetype = lineLab), lwd = 1) +
            geom_point(o_df, mapping = aes(x = Ability, y = prop, shape = !!sym(DIFVar), fill = !!sym(DIFVar),
                                           colour = !!sym(DIFVar)), size = 4)  +
            geom_line(o_df, mapping = aes(x = Ability, y = prop, colour = !!sym(DIFVar),
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
            scale_y_continuous(breaks=seq(0, 1, .1), limits = c(0, 1)) +
            ggthemes::theme_tufte()

        print(plot)
    }
    dev.off()
}
