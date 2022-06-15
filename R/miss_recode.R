#' miss_recode
#'
#' This function separately recodes embedded missing and trailing missing.

#' @param df Dataframe that contains responses to recode.
#' @param begin The 1st item's order in test, e.g., 7.
#' @param end The last item order in test, e.g., 46.
#' @param code_emd Code used for the first missing response (also called embedded missing) of any test taker. Default is '9'.
#' @param code_trail Code used for missing responses after the first/embedded missing response (also called trailing missing) of any test taker. Default is 'M'.
#' @param to_dgt TRUE if it is needed to convert letter responses to digits. Default is FALSE.
#' @param to_ltr TRUE if it is needed to convert digit responses to letters. Default is FALSE.
#' @return Dataframe with recoded symbols for embedded and trailing missing responses of each test taker.
#' @examples
#' a <- miss_recode(df=elena[10:48], begin=1, end=39)
#' b <- miss_recode(df=elena[10:48], begin=1, end=30)
#' c <- miss_recode(df=elena, begin=10, end=48)
#' d <- miss_recode(df=elena, begin=10, end=40, code_emd='m', to_ltr=T)
#' e <- miss_recode(df=racp, begin=2, end=171)
#' @export

miss_recode <- function(df, begin, end, code_emd='9', code_trail='r',
                        to_dgt=FALSE, to_ltr=FALSE) {
    # df <- df %>% modify_at(c(begin:end), as.character)
    N <- ncol(df)
    rwnms <- rownames(df)
    clnms <- colnames(df)

    df_recode <- df[begin:end] %>%
        modify_at(1:ncol(.), as.character)
    df_recode[df_recode=='@@'] <- '9'
    df_recode[df_recode=='@@@'] <- '9'
    df_recode[is.na(df_recode)] <- '9'

    conv_vec <- function(vec, code_emd, code_trail){
        end <- length(vec)
        if (all(vec=='9')){
            vec <- ''
        } else if (vec[end]=='9' & vec[(end-1)]!='9'){
            vec[end] <- code_emd
        } else if (vec[end]=='9' & vec[(end-1)]=='9'){
            seq_ns <- rle(vec[(1:end)] %in% '9')$lengths
            last_1 <- end - seq_ns[[length(seq_ns)]] + 1
            vec[last_1] <- code_emd
            vec[(last_1+1):end] <- code_trail
        }
        vec
    }
    df_coded <- apply(df_recode, 1,
                function(x) conv_vec(x, code_emd=code_emd, code_trail=code_trail))
    if ('list' %in% class(df_coded)){
        df_coded <- df_coded %>%
            reduce(rbind) %>%
            as_tibble()
    } else if ('matrix' %in% class(df_coded)){
        df_coded <- df_coded %>%
            as_tibble() %>%
            t() %>%
            as_tibble()
    }

    # merge together
    if (begin==1){
        if (end==N){
            df <- df_coded
        } else {
            df <- cbind(df_coded, df[(end+1):N])
        }
    } else {
        if (end==N){
            df <- df[1:(begin-1)] %>%
                cbind(df_coded)
        } else {
            df <- df[1:(begin-1)] %>%
                cbind(df_coded) %>%
                cbind(df[(end+1):N])
        }
    }
    rownames(df) <- rwnms
    colnames(df) <- clnms

    # for (i in 1:dim(df)[1]){
    #     if (all(df[i, (begin:end)]=='9')){
    #         df[i, (begin:end)] <- ''
    #     } else if (df[i, end]=='9' & df[i, (end-1)]!='9'){
    #         df[i,end] <- code_emd
    #     } else if (df[i, end]=='9' & df[i, (end-1)]=='9'){
    #         seq_ns <- rle(df[i, (begin:end)] %in% '9')$lengths
    #         last_1 <- end - seq_ns[[length(seq_ns)]] + 1
    #         df[i,last_1] <- code_emd
    #         df[i,(last_1+1):end] <- code_trail
    #     }
    # }

    if (to_dgt){
        df <- modify_at(df, begin:end,
                        ~recode(.,`A`='1',`B`='2',`C`='3',
                                `D`='4',`E`='5',`F`='6'))
    }
    if (to_ltr){
        df <- modify_at(df, begin:end,
                        ~recode(.,`1`='A',`2`='B',`3`='C',
                                `4`='D',`5`='E',`6`='F'))
    }
    df
}
