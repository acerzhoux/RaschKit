#' CCC_comments
#'
#' For dichotomous items, examine distractor CCC and add comments if curve is
#' against theory.
#' For polytomous items, add in comments from Function 'itn_poly_comment'.
#' This is associated with test named 'test'.
#'
#' @param test Name of test.
#' @param dFallThr Proportion on last bin above which falling distractor can be flagged.
#' Default is 0.5.
#' @param dRiseThr Proportion on last bin below which rising distractor is unflagged.
#' Default is 0.1.
#' @param ccc_data Data to draw CCC. One element of list output from Function 'CCC_Vernon'.
#' @param iType Dataframe with columns of iNum and itype. One element of list
#' output from Function 'CCC_Vernon'.
#' @return Vector of comments based solely on CCC.
#' @examples
#' comments <- CCC_comments(test='racp', ccc_data=ccc_data, iType=iType)
#' @export

CCC_comments <- function(test, dFallThr=.5, dRiseThr=.1, ccc_data, iType){
  iType <- iType %>% mutate(Comment='')

  # If there is any poly item, get comments
  if ('poly' %in% iType$itype) {
    comment_poly <- itn_poly_comment(test)
  }

  for(i in 1:nrow(iType)){
    Q_vec <- NULL
    kkk <- ccc_data %>% filter(iNum == iType[i, ]$iNum)

    # dichotomous items
    if (iType[i, ]$itype %in% c('dich', 'score')){
      nBin <- max(as.numeric(kkk$group))
      distrs <- kkk %>% filter(!str_detect(Option, '\\*')) %>%
        pull(Option) %>% unique()

      # check key curve
      keys <- kkk %>%
        filter(group == nBin, str_detect(Option, '\\*')) %>%
        pull(Option) %>% str_sub(1,1)

      for (key in keys) {  #If double key
        prop_key_last <- kkk %>%
          filter(group == nBin, str_detect(Option, key)) %>%
          pull(prop)
        prop_key_prev <- kkk %>%
            filter(group == (nBin-1) & str_detect(Option, key)) %>%
          pull(prop)
        key_incrm <- prop_key_last - prop_key_prev
        if (key_incrm < -.01) {
          Q_vec <- c(Q_vec, paste0('Key ', key, ' falls. Please check key.'))
        } else if (key_incrm < 0.01) {
          if (prop_key_prev < .9) Q_vec <- c(Q_vec,  paste0('Key ', key, ' is flat. Please check key.'))
        }
      }

      if (length(keys) > 1) {
        prop_keys_last <- kkk %>%
          filter(group == nBin, str_detect(Option, '\\*')) %>%
          pull(prop)
        key <- keys[which.min(prop_keys_last)]
        prop_key_last <- min(prop_keys_last)
      }

      dis_ck <- NULL
      miskey <- double_key <- FALSE
      # check distractors
      for (distr in setdiff(setdiff(distrs, keys), c('9', '0'))) {
        a <- kkk %>%
          filter(group == nBin, str_detect(Option, distr)) %>%
          pull(prop)
        b <- kkk %>%
          filter(group == (nBin-1), str_detect(Option, distr)) %>%
          pull(prop)
        incrm <- a - b
        if (a > dRiseThr) {
          if (abs(incrm) <= .01) {
            if (a > dFallThr) {
              if (a > prop_key_last) {
                Q_vec <- c(Q_vec, paste0('More high-ability candidates selected Distractor ', distr, ' rather than key ', key, '.'))
                dis_ck <- c(dis_ck, distr)
              } else {
                Q_vec <- c(Q_vec, paste0('Reasonable proportion of high-ability candidates drawn to Distractor ', distr, '.'))
                dis_ck <- c(dis_ck, distr)
              }
            } else {
              Q_vec <- c(Q_vec, paste0('Distractor ', distr, ' is flat.'))
              dis_ck <- c(dis_ck, distr)
            }
          } else if (incrm > .01) {
            Q_vec <- c(Q_vec, paste0('Distractor ', distr, ' rises.'))
            dis_ck <- c(dis_ck, distr)
            # check whether it's miskeyed or double key
            if (a > prop_key_last) {
              if (key_incrm <= 0)  miskey <- TRUE
              if (key_incrm > .01) double_key <- TRUE
            }
          }
        }
      }

      dis_n <- length(dis_ck)
      if (dis_n == 1){
        Q_vec <- c(Q_vec, paste0('Please ensure ', dis_ck, ' is incorrect.'))
      } else if (dis_n == 2) {
        Q_vec <- c(Q_vec, paste0('Please ensure ', paste(dis_ck, collapse=' and '),
                     ' are incorrect.'))
      } else if (dis_n > 2){
        Q_vec <- c(Q_vec, paste0('Please ensure ', paste(dis_ck[1:(dis_n-1)], collapse=', '), ', and ',
                     dis_ck[[dis_n]], ' are incorrect.'))
      }

      # add 'miskey' into comments
      if (miskey) Q_vec <- c('Miskey?', Q_vec)
      # add 'double key'
      if (double_key) {
        if (length(keys) == 1) {
          Q_vec <- c('Double key?', Q_vec)
        } else if (length(keys) == 2) {
          Q_vec <- c('Triple key?', Q_vec)
        } else if (length(keys) == 3) {
          Q_vec <- c('Quadruple key?', Q_vec)
        } else if (length(keys) == 4) {
          Q_vec <- c('Quintuple key?', Q_vec)
        } else if (length(keys) == 5) {
          Q_vec <- c('Sextuple key?', Q_vec)
        } else if (length(keys) == 6) {
          Q_vec <- c('Septuple key?', Q_vec)
        } else if (length(keys) == 7) {
          Q_vec <- c('Octuple key?', Q_vec)
        } else {
          Q_vec <- c('Multiple key?', Q_vec)
        }
      }
      Q_comments <- paste(Q_vec, collapse=' ')
      iType[i, 'Comment'] <- Q_comments
    }

    # MC items with all-wrong responses
    if (iType[i, ]$itype %in% 'allwrong'){
      nBin <- max(as.numeric(kkk$group))
      opt_all_wrong <- unique(kkk$Option)
      key_pobl <- NULL
      if (length(opt_all_wrong)==1){
        key_pobl <- opt_all_wrong
      } else {
        for (j in opt_all_wrong){
          cond <- {kkk %>% filter(Option==j, group==nBin) %>% pull(prop)} -
                  {kkk %>% filter(Option==j, group==nBin-1) %>% pull(prop)} > .05
          if (cond) key_pobl <- c(key_pobl, j)
        }
      }
      if (!is.null(key_pobl)) iType[i, 'Comment'] <- paste0('Miskey? Please ensure ', key_pobl, ' is incorrect.')
    }

    # polytomous items
    if (iType[i, ]$itype %in% 'poly'){
      if (iType[i, ]$iNum %in% comment_poly$qOrder){
        iType[i, 'Comment'] <- filter(comment_poly, qOrder==iType[i, ]$iNum)$Comment
      }
    }

  }

  # return dataframe with 3rd column as comments
  iType
}
