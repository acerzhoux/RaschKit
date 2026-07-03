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
#' @param sort_cats sort category columns in wide output.
#' @param na_label label to use for NA category in outputs.
#' @param digits rounding digits for proportions.
#' @param freq_suffix suffix for frequency columns in wide output.
#' @param prop_suffix suffix for proportion columns in wide output.
#' @return Table of each item's category frequencies.
#' @examples
#' freq_resps_cat(resp, TRUE, TRUE)
#' @export

freq_resps_cat <- function(
    resp, wide = FALSE, prop = FALSE, sort_cats = TRUE,
    na_label = "(NA)", digits = 2,
    freq_suffix = "_freq", prop_suffix = "_prop"
) {
  stopifnot(is.data.frame(resp))
  resp <- tibble::as_tibble(resp)

  item_lookup <- tibble::tibble(
    Item = names(resp),
    qOrder = seq_along(resp)
  )

  long <- resp |>
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "Item",
      values_to = "Cat"
    ) |>
    dplyr::mutate(
      Cat = trimws(as.character(.data$Cat)),   # clean first
      Cat = dplyr::case_when(
        is.na(Cat) ~ na_label,
        Cat == ""  ~ "(blank)",
        TRUE       ~ Cat
      )
    ) |>
    dplyr::count(.data$Item, .data$Cat, name = "Freq") |>
    dplyr::left_join(item_lookup, by = "Item") |>
    dplyr::relocate(qOrder, .before = Item)

  # ---- long output ----
  if (!wide) {
    if (prop) {
      long <- long |>
        dplyr::group_by(qOrder) |>
        dplyr::mutate(
          Proportion = round(.data$Freq / sum(.data$Freq) * 100, digits)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(qOrder, Item, Cat, Freq, Proportion)
    }
    return(long)
  }

  # ---- wide output ----
  wide_df <- tidyr::pivot_wider(
    long,
    names_from = "Cat",
    values_from = "Freq",
    values_fill = NA
  )

  cats <- setdiff(names(wide_df), c("qOrder", "Item"))

  if (sort_cats && length(cats)) {
    suppressWarnings(nums <- as.numeric(cats))
    is_non_num <- is.na(nums)
    cats <- c(cats[order(nums, na.last = NA)], sort(cats[is_non_num]))
  }

  wide_df <- wide_df |>
    dplyr::select(qOrder, Item, dplyr::all_of(cats)) |>
    dplyr::mutate(
      N = rowSums(dplyr::across(dplyr::all_of(cats)), na.rm = TRUE)
    )

  if (!prop) return(wide_df)

  prop_cols <- wide_df |>
    dplyr::transmute(
      dplyr::across(
        dplyr::all_of(cats),
        ~ round(.x / N * 100, digits),
        .names = paste0("{.col}", prop_suffix)
      )
    )

  wide_df <- wide_df |>
    dplyr::rename_with(
      ~ paste0(.x, freq_suffix),
      dplyr::all_of(cats)
    )

  dplyr::bind_cols(wide_df, prop_cols) |>
    dplyr::select(qOrder, Item, N, tidyselect::everything())
}
