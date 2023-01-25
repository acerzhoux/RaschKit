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
    lookup <- tibble(Parameters=cqs$gXsiParameterLabels %>%
        unlist() %>%
        str_remove('item ') %>%
        str_remove( ' category ') %>%
        str_trim(),
        stat=str_c('Xsi', 1:length(Parameters))
    )

    # Extract estimation history from the  system files
    cq_hist <- cqs %>%
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
        filter(val==min(val)) |>
        tail(1)

    p1 <- ggplot(data=cq_hist %>%
                   mutate(min_lik_iter=min_lik$Iter)%>%
                   filter(Iter > min_lik_iter-150) %>%
                   filter(stat_group=='Xsi') %>%
                     left_join(lookup, by='stat'),
               aes(x=Iter, y=Change, color=Parameters, group=Parameters))
    if (nrow(lookup) > 100) {
        p1 <- p1 + geom_line(show.legend = FALSE)
    } else {
        p1 <- p1 + geom_line()
    }
    p1 <- p1 +
        ggthemes::theme_tufte() +
        labs(title=str_c('(Max) Change: Deltas of ', test))

    plot_dat <- cq_hist %>%
                      filter(stat_group != "Xsi") %>%
                      group_by(stat_group) %>%
                      nest() %>%
        filter(stat_group!="RunNo") %>%
        mutate(min_lik_iter=min_lik$Iter ) %>%
        unnest(data)%>%
        mutate(min_lik_iter=min_lik$Iter )%>%
        filter(Iter > min_lik_iter-150)

    df_p2 <- plot_dat %>%
        filter(stat_group=='Beta_Est1_D1')
    p2 <- ggplot(data=df_p2, aes(x=Iter, y=Change)) +
        geom_line()+
        ggthemes::theme_tufte() +
        labs(title=str_c('(Max) Change: Regressors of ', test))

    p3 <- ggplot(data=plot_dat %>%
                   filter(stat_group=='Likelihood'),
               aes(x=Iter, y=Change)) +
        geom_line() +
        ggthemes::theme_tufte() +
        labs(title=str_c('(Max) Change: Deviance of ', test))

    p4 <- ggplot(data=plot_dat %>%
                   filter(stat_group=='Variance_D1'),
               aes(x=Iter, y=Change)) +
        geom_line()+
        ggthemes::theme_tufte() +
        labs(title=str_c('(Max) Change: Covariance of ', test))

    if (all(df_p2$val==0)){
        plot_ls <- list(p1, p3, p4)
    } else {
        plot_ls <- list(p1, p2, p3, p4)
    }

    pdf(file=paste0('output/', test, "_Convergence_check.pdf"), width = 10, height = 7)
    map(plot_ls, ~print(.x))
    dev.off()
}
