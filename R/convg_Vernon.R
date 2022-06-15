#' convg_Vernon
#'
#' This function checks convergence of the model by checking model deviance change and maximum changes in estimates of delta's, covariance, and regressors. This is associated with test named 'test'. A plot of the checks is saved in 'output' folder in the working directory.
#'
#' @param test Name of test.
#' @param folder Where the ConQuest output .cqs file is located. Default is 'output' folder in working directory.
#' @examples
#' convg_Vernon(test='racp1')
#' @export

convg_Vernon <- function(folder=here::here('output'), test){
    path_cqs <- file.path(folder, paste0(test, '.cqs'))

    # Extract estimation history from the  system files
    cq_hist <- ConQuestSys(path_cqs) %>%
        getCqHist() %>%
        as_tibble() %>%
        pivot_longer(cols=-Iter,
                     names_to="stat",
                     values_to="val") %>%
        na.omit() %>% # bec only one plot for the changes Xsi/item pars will be created
        mutate(stat_group=ifelse(str_detect(stat, "Xsi"), "Xsi", stat)) %>%
        arrange(stat, Iter) %>%
        group_by(stat) %>%
        mutate(prev_val=lag(val),
               Change=case_when(
                   stat %in% c("Likelihood") ~ prev_val-val,
                   TRUE ~ val-prev_val))

    # Prepare plot data
    min_lik <- cq_hist %>%
        filter(stat=="Likelihood") %>%
        ungroup() %>%
        filter(val==min(val))

    plot_dat <- cq_hist %>%
        filter(stat_group=="Xsi") %>%
        group_by(stat_group) %>%
        nest() %>%
        mutate(data=map(data,
              ~ mutate(.x, stat=str_remove(stat, "Xsi") %>% as.numeric()) %>%
              arrange(stat, Iter) %>%
              group_by(Iter) %>%
              filter(abs(Change)==max(abs(Change))) %>%
              ungroup() %>%
              arrange(Iter) %>%
              mutate(stat=as.character(stat)))) %>%
        bind_rows(cq_hist %>%
                      filter(stat_group != "Xsi") %>%
                      group_by(stat_group) %>%
                      nest()) %>%
        filter(stat_group!="RunNo") %>%
        mutate(label=case_when(
            stat_group=="Likelihood" ~ "I. Deviance Change",
            stat_group=="Xsi" ~ "II. Deltas Max Change",
            stat_group=="Variance_D1" ~ "III. Covariance Max Change",
            stat_group=="Beta_Est1_D1" ~ "IV. Regressors Max Change")) %>%
        mutate(min_lik_iter=min_lik$Iter )

    # Change Plots
    p_save <- ggplot(data=plot_dat %>%
                         unnest(data) %>%
                         filter(Iter > min_lik_iter-150),
                     aes(x=Iter, y=Change)) +
        geom_hline(yintercept=0, colour="purple", alpha=.5) +
        geom_point() +
        geom_line() +
        facet_wrap(label~.) +
        ggthemes::theme_tufte() +
        labs(x="Iteration",
             y='',
             title=paste0(test, ': Convergence Check'))

    print(p_save)
    ggsave(here::here(folder, paste0(test, '_Convergence_check.pdf')), p_save,
           width=20, height=20, units="cm")
}
