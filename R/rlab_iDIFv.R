#' rlab_iDIFv
#'
#' This function relabel DIF items that show up from vertical equating.
#'
#' This function uses vertical equating results (DIF_ls) to relabel each grade's item labels (lab_ls) and merge all grades' data into one. Second, it adds a dummy regressor for each grade except the lowest grade. Third, it saves merged data, revised labels, and keys into 'Data' folder. Finally, it returns the column numbers of pid, responses, and grade regressors to be used for 'test.cqc' file in 'input' folder.
#'
#' @param test Name of test.
#' @param pid Variable name of person ID.
#' @param grade Variable name of students' grade.
#' @param data_ls List of dataframe for each grade. Name is grade, e.g., `2`. Each dataframe has two parts, covariates (include `StudentID`, `grade`) and responses.
#' @param lab_ls List of lables. Name is grade, e.g., `2`. Element is vector of item labels (char).
#' @param cd_ls List of codebook dataframes. Name is grade. Element is dataframe with variables of `Item ID`, `Key`. Make sure item order is same as item response order in data_ls.
#' @param DIF_ls List of DIF item labels (char). Name is grade. Element is DIF item labels.
#' @param n_cov Number of covariates before responses.
#' @return List of column numbers of pid, responses, and grade regressors.
#' @examples
#' rlab_iDIFv()
#' @export

rlab_iDIFv <- function(test, pid, grade, data_ls, lab_ls, cd_ls, DIF_ls, n_cov){
    grd_min <- names(lab_ls) %>% parse_number() %>% min()
    grd_max <- names(lab_ls) %>% parse_number() %>% max()

    for (i in names(DIF_ls)){
        iRecode <- DIF_ls[[i]]
        j <- parse_number(i)
        a <- j - (grd_min - 1)
        b <- a + 1
        lab_ls[[a]][which(lab_ls[[a]] %in% iRecode)] <- paste0(iRecode,'_','G', j)
        lab_ls[[b]][which(lab_ls[[b]] %in% iRecode)] <- paste0(iRecode,'_','G', (j+1))
        cd_ls[[a]] <- cd_ls[[a]] %>%
            mutate(`Item ID`=lab_ls[[a]])
        cd_ls[[b]] <- cd_ls[[b]] %>%
            mutate(`Item ID`=lab_ls[[b]])
    }

    # update codebook
    codebook_merged <- reduce(cd_ls, full_join)

    # update vertical DIF item labels each grade
    vars <- names(data_ls[[1]])[1:n_cov]
    df_relabeled <- map2(data_ls, lab_ls, ~`colnames<-`(.x, c(vars, .y))) %>%
        map(~select(., vars, -contains('..'), -contains('...')))

    df_merged <- reduce(df_relabeled, bind_rows) %>%
        modify_at(1, as.character) %>%
        modify_at(-1, as.character)
    n_resp <- length(df_merged) - n_cov

    # add grade regressors
    grades <- map((grd_min+1):grd_max, ~paste0('G', .)) %>%
        reduce(c)
    grd_vars <- map((grd_min+1):grd_max,
        ~paste0('G', ., '=ifelse(', grade, '==\'', ., '\'', ', 1, 0)')) %>%
        lazyeval::as.lazy_dots()
    df_grds <- df_merged %>% mutate_(.dots=grd_vars)
    names(df_grds)[(n_cov+n_resp+1):(n_cov+n_resp+grd_max-grd_min)] <- grades
    df_grds[is.na(df_grds)] <- ''

    # update codebook
    cd_merged <- reduce(map(cd_ls, ~select(., 'Key', 'Item ID')), full_join)

    # save data, labels, keys into 'Data' folder
    data_into_Data(test=test, data=df_grds) #274 items
    labels_into_Data(test=test, labels=cd_merged %>% pull(`Item ID`))
    keys_into_Data(test=test, keys=cd_merged %>% pull(Key))

    pid_resp_regrs_cols(df=df_grds, pid=pid, n_cov=n_cov,
                        n_resp=n_resp, regr_vec=grades)
}
