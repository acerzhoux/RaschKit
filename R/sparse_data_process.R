#' sparse_data_process
#'
#' This function preprocesses data in three ways. First, convert non-used
#' categories and missing values to NA. Second, remove item responses with
#' all missing values. Third, for DIF analysis, remove items that have no data
#' on any category of the DIF variable. This is associated with test named 'test'.
#' An Excel file with processed data and removed items will be saved to 'data'
#' folder in working directory.
#'
#' @param test Name of test.
#' @param data Data to be processed.
#' @param keys Keys to the test items.
#' @param labels Item labels.
#' @param n_cov Number of covariables before reponses in the dataframe.
#' @param n_dims Vector of number of items for each dimension in the test.
#' @param miss_code Code for missing data that should be coded to NA.
#' @param DIFVar Name of DIF variable. Should be lowercase for ConQuest to run.
#' Default is NULL.
#' @return List of processed data, updated dimension test numbers, updated keys,
#' and updated labels.
#' @examples
#' processed <- sparse_data_process(test='Dan', data=df_DIF, keys=rep(1,1072),
#' labels=rep(1,1072), n_cov=5, n_dims=c(901, 171),
#' miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
#' DIFVar='studGender')
#' processed1 <- sparse_data_process(test='Dan', data=df_DIF, keys=rep(1,1072),
#' labels=rep(1,1072), n_cov=5, n_dims=c(901, 171),
#' miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '))
#' processed_elena <- sparse_data_process(test='elena', data=elena,
#' keys=rep(1,39), labels=rep(1,39), n_cov=9, n_dims=c(30, 9),
#' miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '), DIFVar='gender')
#' @export

sparse_data_process <- function(test, data, keys, labels, n_cov, n_dims,
                                miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
                                DIFVar=NULL){
    nms_orig <- names(data)[-c(1:n_cov)]

    # recode miss symbols to NA
    if (!is.null(miss_code)){
        for (i in miss_code) data[data==i] <- NA
    }

    # remove item responses with all missing values
    vars_miss <- names(data)[which(colSums(is.na(data))==nrow(data))]
    data <- missAll_remove_cols(data)

    # change key of all-correct-response items to 'x'?

    # remove items with no data on any category of DIF variable
    if (!is.null(DIFVar)) {
        data <- data[!is.na(data[DIFVar]), ]
        N_DIFVar_cat <- length(unique(data[[DIFVar]]))

        vars_miss_DIFVar <- data %>%
            split(data[DIFVar]) %>%
            map(~select(., (n_cov+1):length(data))) %>%
            map2(rep(list(keys), N_DIFVar_cat), ~colSums((is.na(.x)|.x==.y))== nrow(.x) |
                     colSums((is.na(.x)|.x!=.y))== nrow(.x)) %>%
            map(., ~which(.)) %>%
            reduce(c) %>%
            names() %>%
            unique()
        data <- data %>%
            select(-all_of(vars_miss_DIFVar))
        items_rmved <- c(vars_miss, vars_miss_DIFVar)
    } else{
        items_rmved <- vars_miss
    }

    # removed items
    items_rmved_order <- which(nms_orig %in% items_rmved)
    n_dims_new <- vector()
    for (i in 1:length(n_dims)){
        if (i==1){
            n_dims_new[i] <- n_dims[i] - length(which(items_rmved_order %in% 1:n_dims[[i]]))
        }
        if (i>1){
            n_dims_new[i] <- n_dims[i] -
                length(which(items_rmved_order %in% (sum(n_dims[1:(i-1)])+1):sum(n_dims[1:i])))
        }
    }
    Removed <- tibble(Item=vars_miss, Reason='No data')
    if (!is.null(DIFVar)) {
        Removed <- Removed %>%
            bind_rows(tibble(Item=vars_miss_DIFVar,
                             Reason='No data for at least one DIF category'))
    }

    # when no item is removed
    Removed <- Removed %>%
        filter(!is.na(Item))
    if (!identical(items_rmved_order, integer(0))){
        keys <- keys[-items_rmved_order]
        labels <- labels[-items_rmved_order]
    }

    # ####### save data, return data and process ways
    if (!is.null(DIFVar)) {
        writexl::write_xlsx(list(data=data, Removed=Removed),
                            paste0('data/', test, '_', DIFVar, '.xlsx'))
    }

    list(data=data,
         n_dims=n_dims_new,
         keys=keys,
         labels=labels)
}
