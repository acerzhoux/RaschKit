#' response_repeat
#'
#' This function extracts candidates who have a minimum number of repetitions of option(s).

#' @param df Dataframe that contains responses.
#' @param begin The 1st item's order in test, e.g., 7.
#' @param end The last item order in test, e.g., 46.
#' @param codes Option(s) to check for repetition, e.g., c('A','C')
#' @param rep_min Minimum number of repetition, e.g., 8.
#' @return Subset of input responses with a minimum number of repetitions of option(s).
#' @examples
#' a <- response_repeat(df=elena[10:48], begin=1, end=39)
#' @export

# candidates who repeated responses
response_repeat <- function(df, begin, end, codes, rep_min){
    rep_flag <- function(resp_row, rep_min, code){
        id <- which(rle(resp_row %in% code)$values)
        any(rle(resp_row %in% code)$lengths[id] >= rep_min)
    }

    id_rep <- map(codes,
                  ~apply(df[begin:end], 1,
                         function(x) rep_flag(x, rep_min=rep_min, code=.x))) %>%
        reduce(bind_cols) %>%
        apply(1, function(x) any(x))

    bind_cols(df, tibble(flag=id_rep)) %>%
        filter(flag==TRUE) %>%
        select(-flag)
}
