#' freq_resps_cat
#'
#' This function . This is associated with test named 'test'.
#'
#' @param resp Dataframe of responses where each column is a vector of responses
#' to one test item.
#' @param wide TRUE if wide form is desired. Default is FALSE. When default,
#' the output has long format same as frequency table in .txt file output
#' from ConQuest.
#' @param prop TRUE if proportion is desired. Default is FALSE.
#' @return Table of each item's category frequencies.
#' @examples
#' freq_resps_cat(resp, TRUE, TRUE)
#' @export

freq_resps_cat <- function(resp, wide = FALSE, prop = FALSE) {
  item_lookup <- tibble(Item = names(resp), qOrder = 1:ncol(resp))
  frequencies <- left_join(
    dplyr::select(
      dplyr::rename(
        reduce(
          imap(
            map(map(resp, ~table(.x, useNA = "always")), ~as.data.frame(.)),
            ~mutate(.x, Item = .y)
          ),
          bind_rows
        ), Cat = .x
      ),
      Item,
      everything()
    ),
    item_lookup,
    by = "Item"
  )

  if (wide) {
    frequencies <- pivot_wider(frequencies, names_from = "Cat", values_from = "Freq")
    cats <- setdiff(names(frequencies), c("Item", "qOrder"))
    if (any(c("A", "B", "C", "D") %in% cats)) {
      cats <- sort(cats)
    } else {
      idNA <- which(is.na(as.numeric(cats)))
      if (identical(idNA, integer(0))) {
        cats <- as.character(sort(as.numeric(cats)))
      } else {
        cats <- c(sort(as.numeric(cats[setdiff(1:length(cats), idNA)])), cats[idNA])
      }
    }
    frequencies <- dplyr::select(frequencies, qOrder, Item, all_of(cats))
    N <- rowSums(frequencies[-c(1, 2)], na.rm = TRUE)
    frequencies <- cbind(frequencies, N)
    if (prop) {
      frequencies <- frequencies |>
        left_join(
          mutate(frequencies, across(-c("qOrder", "Item", "N"), ~round(.x/N * 100, 2))),
          by=c('qOrder', 'Item', 'N')
        ) |>
        select(qOrder, Item, N, everything())

      # change .x, .y in names
      names(frequencies) <- gsub("\\.x", '_freq', names(frequencies))
      names(frequencies) <- gsub("\\.y", '_prop', names(frequencies))
    }

    frequencies
  } else {
    if (prop) {
      frequencies <- frequencies |>
        group_by(qOrder) |>
        mutate(
          Proportion=round(Freq/sum(Freq) * 100, 2)
        ) |>
        ungroup() |>
        select(qOrder,Item,Cat,everything())
    }
    frequencies
  }
}
