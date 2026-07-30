#' Perform Horizontal or Vertical Equating
#'
#' Conducts DIF-based equating between pairs of test administrations.
#' The function supports both:
#' \itemize{
#'   \item \strong{Horizontal equating (Hrz)}: comparisons among forms within
#'   the same grade.
#'   \item \strong{Vertical equating (Vrt)}: comparisons between adjacent grades.
#' }
#'
#' For each comparison, the function performs anchor analysis, generates DIF
#' statistics and diagnostic plots, and saves the results to Excel workbooks
#' and image files in the \code{equating} folder.
#'
#' @param linkTypeLst A length-one list defining the equating design.
#'   Supported formats include:
#'   \itemize{
#'     \item \code{list(Hrz = list(grades = 2:10,
#'       forms = c("A", "B", "C")))}
#'       for horizontal equating across all pairwise form combinations within
#'       each specified grade.
#'
#'     \item \code{list(Hrz = list(grades = NULL,
#'       forms = c("A", "B", "C")))}
#'       for horizontal equating across forms without grade prefixes.
#'
#'     \item \code{list(Vrt = 2:10)}
#'       for vertical equating between adjacent grades (e.g., 2-3, 3-4,
#'       ..., 9-10).
#'   }
#'
#' @param test Character string specifying the test name.
#' @param p_cut Significance level for the chi-square anchor-item screening
#'   procedure. Default is \code{0.05}.
#' @param DIF_cut Absolute delta difference threshold used to flag potential
#'   DIF items. Default is \code{0.5}.
#' @param DIF_std_cut Standardized delta difference threshold used to flag
#'   potential DIF items. Default is \code{4}.
#' @param step Logical. If \code{TRUE}, DIF analysis is performed on step
#'   parameters rather than item locations. Default is \code{FALSE}.
#' @param iter Logical. If \code{TRUE}, DIF items are iteratively removed and
#'   the analysis is repeated. Default is \code{TRUE}.
#' @param sigma Logical. If \code{TRUE}, \code{delta.y} is linearly transformed
#'   to match both the mean and standard deviation of \code{delta.x}. If
#'   \code{FALSE}, only the mean is matched. Default is \code{FALSE}.
#' @param run Character string identifying the analysis run (e.g.,
#'   \code{"pre_review"}, \code{"post_review"}).
#'
#' @return A named list containing equating results for each comparison,
#'   including DIF statistics, anchor information, transformation coefficients,
#'   and summary tables.
#'
#' @examples
#' # Horizontal equating across all pairwise form combinations
#' equate2Type(
#'   list(Hrz = list(grades = NULL, forms = c("B", "C", "D"))),
#'   test = "OPRA"
#' )
#'
#' # Vertical equating between adjacent grades
#' equate2Type(
#'   list(Vrt = 2:10),
#'   test = "OPRA"
#' )
#'
#' @export

equate2Type <- function(
    linkTypeLst,
    test,
    p_cut = 0.05,
    DIF_cut = 0.5,
    DIF_std_cut = 4,
    step = FALSE,
    iter = TRUE,
    sigma = FALSE,
    run
) {

  #---------------------------------------------------------------------------
  # Check inputs
  #---------------------------------------------------------------------------

  if (!is.list(linkTypeLst) ||
      length(linkTypeLst) != 1 ||
      !(names(linkTypeLst) %in% c("Hrz", "Vrt"))) {

    stop(
      "linkTypeLst should be a length-one list such as
      list(Hrz=list(grades=c(2:10), forms=c('A','B','C'))) or
      list(Vrt=c(2:10))."
    )
  }

  type <- names(linkTypeLst)
  obj  <- linkTypeLst[[type]]

  if (type == "Hrz" &&
      !all(c("grades", "forms") %in% names(obj))) {
    stop(
      "For Hrz, linkTypeLst element must contain 'grades' and 'forms'."
    )
  }

  #---------------------------------------------------------------------------
  # Extract settings
  #---------------------------------------------------------------------------

  if (type == "Hrz") {
    grdIntVec <- obj$grades
    forms     <- obj$forms
  } else {
    grdIntVec <- obj
  }

  var_name    <- if (type == "Hrz") "" else "L"
  name_prefix <- if (type == "Hrz") " " else " L"

  folder <- file.path(
    "equating",
    paste0(type, "_", if (step) "step_", test)
  )

  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  #---------------------------------------------------------------------------
  # Build comparison groups
  #---------------------------------------------------------------------------

  if (type == "Hrz") {

    cmbs <- combn(forms, 2, simplify = FALSE)

    grps <- if (is.null(grdIntVec)) {

      cmbs

    } else {

      unlist(
        lapply(grdIntVec, function(g) {

          setNames(
            lapply(cmbs, function(x) paste0(g, x)),
            paste0(
              g,
              "_",
              sapply(cmbs, paste0, collapse = "")
            )
          )

        }),
        recursive = FALSE
      )
    }

  } else {

    grps <- Map(
      c,
      grdIntVec[-length(grdIntVec)],
      grdIntVec[-1]
    )

  }

  #---------------------------------------------------------------------------
  # Cache item statistics (avoids repeated df_its calls)
  #---------------------------------------------------------------------------

  if (!step) {

    all_tests <- unique(unlist(grps))

    its_cache <- setNames(
      lapply(
        all_tests,
        function(x) df_its(paste0(test, "_", x), run)
      ),
      all_tests
    )

  }

  #---------------------------------------------------------------------------
  # Helper for adding facility/discrimination columns
  #---------------------------------------------------------------------------

  add_item_stats <- function(df, item_stats) {

    left_join(
      df,
      item_stats,
      by = c("item" = "Label")
    ) |>
      dplyr::select(
        item,
        contains("N"),
        contains("Facil"),
        contains("Discr"),
        contains("Fitw"),
        everything()
      )
  }

  #---------------------------------------------------------------------------
  # Run equating
  #---------------------------------------------------------------------------

  equat_ls <- list()

  for (vars in grps) {

    grp_name <- paste(vars, collapse = "_")
    prefix   <- file.path(folder, grp_name)

    t1 <- paste0(test, "_", vars[1])
    t2 <- paste0(test, "_", vars[2])

    #-----------------------------------------------------------------------
    # Facility/discrimination summary
    #-----------------------------------------------------------------------

    if (!step) {

      facilDiscrFitw <- its_cache[[as.character(vars[1])]] |>
        inner_join(
          its_cache[[as.character(vars[2])]],
          by = "Label"
        ) |>
        dplyr::select(-contains("iNum.")) |>
        na.omit() |>
        modify_at(c(3, 7), round, 3)

      names(facilDiscrFitw) <- gsub(
        "\\.x",
        paste0(name_prefix, vars[1]),
        names(facilDiscrFitw)
      )

      names(facilDiscrFitw) <- gsub(
        "\\.y",
        paste0(name_prefix, vars[2]),
        names(facilDiscrFitw)
      )
    }

    #-----------------------------------------------------------------------
    # Equating
    #-----------------------------------------------------------------------

    statsEqu <- Equate_shw(
      test,
      vars,
      var_name,
      p_cut,
      DIF_cut,
      DIF_std_cut,
      FALSE,
      step,
      iter,
      sigma,
      run
    )

    ggsave(
      paste0(prefix, "_delta.png"),
      statsEqu[["plot_DIF"]],
      width = 17,
      height = 30,
      units = "cm"
    )

    #-----------------------------------------------------------------------
    # Additional outputs
    #-----------------------------------------------------------------------

    if (!step) {

      ggsave(
        paste0(prefix, "_facilDiscrFitw.png"),
        plot_facilDiscrFitw(
          facilDiscrFitw,
          paste0(var_name, vars),
          c(3, 7),
          3
        ),
        width = 17,
        height = 30,
        units = "cm"
      )

      statsEqu$flag  <- add_item_stats(statsEqu$flag, facilDiscrFitw)
      statsEqu$final <- add_item_stats(statsEqu$final, facilDiscrFitw)
    }

    writexl::write_xlsx(
      statsEqu[1:5],
      paste0(prefix, "_process.xlsx")
    )

    equat_ls[[grp_name]] <- statsEqu
  }

  #---------------------------------------------------------------------------
  # Summary
  #---------------------------------------------------------------------------

  summary <- map(equat_ls, "shift") |>
    imap(~ mutate(.x, Grade = .y)) |>
    map2(
      map(equat_ls, "final"),
      ~ mutate(
        .x,
        Links_bfr = nrow(.y),
        Links_afr = nrow(filter(.y, flag == 0)),
        Links_retained_perc =
          paste0(round(Links_afr / Links_bfr * 100), "%")
      )
    ) |>
    reduce(bind_rows) |>
    select(Grade, everything())

  ls_save <- list(Summary = summary) |>
    append(map(equat_ls, "final"))

  #---------------------------------------------------------------------------
  # Save workbook
  #---------------------------------------------------------------------------

  file <- file.path(
    "equating",
    paste0(type, "_", if (step) "step_", test, ".xlsx")
  )

  cat(
    "\n",
    test,
    type,
    "equating results saved at:\n\t",
    file
  )

  add_format()[['equate']](
    ls_save,
    folder,
    file,
    c(DIF_cut, DIF_std_cut)
  )

}
