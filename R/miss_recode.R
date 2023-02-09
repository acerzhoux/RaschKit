#' miss_recode
#'
#' This function recodes embedded missing, trailing missing, and all missing
#' with code given.

#' @param df Dataframe that contains responses.
#' @param begin The 1st item's order in test, e.g., 7.
#' @param end The last item order in test, e.g., 46.
#' @param code_emd Code used for the first missing response (also called
#' embedded missing) of any test taker. Default is '9'.
#' @param code_trail Code used for missing responses after the first/embedded
#' missing response (also called trailing missing) of any test taker. Default is 'M'.
#' @param code_allMiss Code to use for rows of all missing values.
#' Default is NA_character_.
#' @param miss_code Missing codes that need to be converted to embedded and
#' trailing missing symbols such as 'M' and 'R'. Commonly used missing symbols are
#' '@@' (less or more),'7','8','9','88','99','.','',' ', and '-'.
#' @param to_dgt TRUE if it is needed to convert letter responses to digits.
#' Default is FALSE.
#' @param to_ltr TRUE if it is needed to convert digit responses to letters.
#' Default is FALSE.
#' @return Dataframe with recoded symbols for embedded and trailing missing
#' responses of each test taker.
#' @export

miss_recode <- function(df, begin, end, code_emd='M', code_trail='R',
                        code_allMiss=NA_character_,
                        miss_code=c('@','@@','@@@','@@@@','7','8','9','88','99','.','',' ', '-'),
                        to_dgt=FALSE, to_ltr=FALSE) {
    # df <- df %>% modify_at(c(begin:end), as.character)
    N <- ncol(df)
    rwnms <- rownames(df)
    clnms <- colnames(df)

    df_recode <- df[begin:end] %>%
        modify_at(1:ncol(.), as.character)

    # convert missing code to embedded missing code
    for (ms in  miss_code){
        df_recode[df_recode==ms] <- code_emd
    }
    df_recode[is.na(df_recode)] <- code_emd

    conv_vec <- function(vec, code_emd, code_trail, code_allMiss){
        end <- length(vec)
        if (all(vec==code_emd)){
            vec <- rep(code_allMiss, length(vec))
        } else if (vec[end]==code_emd & vec[(end-1)]!=code_emd){
            # vec[end] <- code_emd
            # next
        } else if (vec[end]==code_emd & vec[(end-1)]==code_emd){
            seq_ns <- rle(vec[(1:end)] %in% code_emd)$lengths
            last_1 <- end - seq_ns[[length(seq_ns)]] + 1
            vec[last_1] <- code_emd
            vec[(last_1+1):end] <- code_trail
        }
        vec
    }
    df_coded <- apply(df_recode, 1,
                function(x) conv_vec(x, code_emd=code_emd,
                                     code_trail=code_trail,
                                     code_allMiss=code_allMiss))
    # if ('list' %in% class(df_coded)){
    #     df_coded <- df_coded %>%
    #         reduce(bind_rows(.)) %>%
    #         as_tibble(.name_repair = "minimal")
    # } else if ('matrix' %in% class(df_coded)){
    df_coded <- df_coded %>%
        as_tibble() %>%
        t() %>%
        as_tibble(.name_repair = "minimal")
    # }

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
