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

freq_resps_cat <- function( # ChatGPT revision
    resp,
    wide       = FALSE,
    prop       = FALSE,
    sort_cats  = TRUE,
    na_label   = "(NA)",
    digits     = 2,
    freq_suffix = "_freq",
    prop_suffix = "_prop"
) {
  stopifnot(is.data.frame(resp))

  resp <- tibble::as_tibble(resp)

  # Column order lookup
  item_lookup <- tibble::tibble(
    Item   = names(resp),
    qOrder = seq_along(resp)
  )

  # Long counts (keeps NA by design; we relabel NA for wide safety)
  long <- resp |>
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to  = "Item",
      values_to = "Cat"
    ) |>
    dplyr::mutate(Cat = dplyr::if_else(is.na(.data$Cat), na_label, as.character(.data$Cat))) |>
    dplyr::count(.data$Item, .data$Cat, name = "Freq") |>
    dplyr::left_join(item_lookup, by = "Item") |>
    dplyr::relocate(qOrder, .before = Item)

  # --- Long format output ---
  if (!wide) {
    if (prop) {
      long <- long |>
        dplyr::group_by(qOrder) |>
        dplyr::mutate(Proportion = round(.data$Freq / sum(.data$Freq) * 100, digits)) |>
        dplyr::ungroup() |>
        dplyr::select(qOrder, Item, Cat, Freq, Proportion)
    }
    return(long)
  }

  # --- Wide format output ---
  wide_df <- long |>
    tidyr::pivot_wider(names_from = "Cat", values_from = "Freq")

  # Identify category columns (everything except qOrder/Item)
  cats <- setdiff(names(wide_df), c("qOrder", "Item"))

  # Sort categories: numeric cats first ascending, then non-numeric alphabetic
  if (sort_cats && length(cats)) {
    suppressWarnings(nums <- as.numeric(cats))
    is_non_num <- is.na(nums)
    cats <- c(cats[order(nums, na.last = NA)], sort(cats[is_non_num]))
  }

  # Reorder and add N (total responses per item)
  wide_df <- wide_df |>
    dplyr::select(qOrder, Item, dplyr::all_of(cats)) |>
    dplyr::mutate(N = rowSums(dplyr::select(dplyr::across(dplyr::all_of(cats)), tidyselect::everything()), na.rm = TRUE))

  # If only frequencies requested
  if (!prop) {
    return(wide_df)
  }

  # Build proportion columns (percent) alongside frequency columns
  prop_cols <- wide_df |>
    dplyr::transmute(
      dplyr::across(
        dplyr::all_of(cats),
        ~ round(.x / N * 100, digits),
        .names = paste0("{.col}", prop_suffix)
      )
    )

  # Suffix frequency columns
  wide_df <- wide_df |>
    dplyr::rename_with(~ paste0(.x, freq_suffix), dplyr::all_of(cats))

  # Bind frequency + proportion, order columns nicely
  out <- dplyr::bind_cols(wide_df, prop_cols) |>
    dplyr::select(qOrder, Item, N, tidyselect::everything())

  return(out)
}
