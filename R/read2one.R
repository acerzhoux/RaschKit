#' read2one
#'
#' This function reads from 'folder' files associated with 'tests'
#' and puts them into one file.
#'
#' @param folder Folder where to-be-read files are located. Must be one of
#' c('results', 'DIF', 'equating').
#' @param tests Vector of test names after '_'.
#' @param prefix Common prefix in file names before first '_'. Default is NULL.
#' @param file_name Name given to new file, each sheet of which is one test's result.
#' Default is NULL.
#' @param vars Vector of length 2 such as c('girls','boys'). Default is NULL.
#' Should be specified when to read multiple DIF analysis (binary DIF variable)
#' Excels and plots from 'DIF' folder.
#' Its order corresponds to the alphabetic/numeric order of DIF variables'
#' two categories in data.
#' @examples
#' # Not run
#' # read2one(folder='results', tests=c('bang_35', 'math_35'), prefix='itn')
#' @export

read2one <- function (folder = c('results', 'DIF', 'equating'), tests, prefix=NULL,
            file_name = NULL, vars=NULL){
  # Excel names to read files from
  if (folder == 'equating'){
    file_ls <- map(tests, ~list.files(folder, pattern=.x, full.names=TRUE))
    files <- file_ls |>
      map(~str_subset(.x, '.xlsx')) |>
      unlist()
    in_dif <- file_ls |>
      map(~str_subset(.x, '.png')) |>
      unlist()
  } else {
    files <- file.path(folder, str_c(prefix, '_', tests, '.xlsx'))
  }

  # sheet name to read from
  sheetNm <- ifelse(folder %in% c('DIF', 'equating'), 'final', 1)
  ex_ls <- map(files, ~readxl::read_xlsx(.x, sheetNm))

  # Excel name to save files into one
  if (folder == 'equating'){
    file_name <- list.files(folder, pattern=tests[[1]]) |>
      str_subset('.xlsx') |>
      strsplit('_') |>
      unlist()
    file_name <- file_name[length(file_name)] |>
      str_sub(1, -6)
    file <- file.path(folder, str_c(file_name, ".xlsx"))
  } else if (!is.null(file_name)) {
    file <- ifelse(
      is.null(prefix),
      file.path(folder, str_c(file_name, '.xlsx')),
      file.path(folder, str_c(prefix, '_', file_name, '.xlsx'))
    )
  } else {
    file <- file.path(folder, str_c(prefix, ".xlsx"))
  }

  # save results: equating, DIF
  if (sheetNm == 'final'){
    # combine excels
    names(ex_ls) <- tests
    if (folder == 'equating'){
      summary <- map(files, ~readxl::read_xlsx(.x, 'shift'))  |>
        map2(tests, ~mutate(.x, Domain=.y)) |>
        map2(map(files, ~readxl::read_xlsx(.x, 'final')),
           ~mutate(.x, Links_bfr=nrow(.y),
               Links_afr=nrow(filter(.y, flag==0)),
               Links_retained_perc=str_c(round(Links_afr/Links_bfr*100), '%'))) |>
        reduce(bind_rows) |>
        select(Domain, everything())
      ls_save <- list(Shift=summary) |>
        append(ex_ls)
    } else {
      summary <- map(ex_ls, ~.x |> filter(flag==1)) |>
        imap(~.x |> mutate(Domain=.y)) |>
        map(~.x |> mutate(favor=as.character(ifelse(DIF<0, vars[[1]], vars[[2]])))) |>
        reduce(bind_rows) |>
        select(Domain, favor, everything()) |>
        arrange(Domain, favor, desc(chisq))
      ls_save <- list(Summary=summary) |>
        append(ex_ls)
    }

    # combine pdf's
    if (folder == 'DIF') {
      in_dif <- file.path(folder, str_c(prefix, '_', tests, '.pdf'))
      out_dif <- file.path(folder, str_c(prefix, '.pdf'))
      qpdf::pdf_combine(input = in_dif, output = out_dif)

      in_facil <- file.path(folder, str_c(prefix, '_', tests, '_Facility.pdf'))
      out_facil <- file.path(folder, str_c(prefix, '_Facility.pdf'))
      qpdf::pdf_combine(input = in_facil, output = out_facil)
    } else {
      # out_dif <- file.path(folder, str_c(file_name, ".pdf"))
      # qpdf::pdf_combine(input = in_dif, output = out_dif)
    }

    # move into folder of 'file_name'
    new_path <- file.path(folder, file_name)
    dir.create(new_path)
    map(
      file.path(
        getwd(), folder,
        c(str_sub(c(in_dif, files), 10, -1),
          list.files(folder, '.html'))
      ),
      ~fs::file_move(path=.x, new_path=new_path)
    )

    # add note sheet
    wb <- createWorkbook()
    addWorksheet(wb, names(ls_save)[[1]])
    writeData(wb, sheet = names(ls_save)[[1]], x = ls_save[[1]])

    # add flagged and test sheets
    for (i in 2:length(ls_save)){
      sheet <- names(ls_save)[[i]]
      n_case <- nrow(ls_save[[i]])+1
      n_col <- ncol(ls_save[[i]])
      addWorksheet(wb, sheet)
      writeData(
        wb,
        sheet = sheet,
        x = ls_save[[i]] |>
          dplyr::mutate(
            `Files`=c(
              file.path(getwd(), folder, file_name, paste0(sheet, '_', file_name, '.html')),
              file.path(getwd(), folder, file_name, paste0(sheet, '_', file_name, '.xlsx')),
              rep(NA, n_case-2-1)
            )
          )
      )

      # header, body style
      wb <- add_format()[['addHeaderStyle']](wb, n_col, sheet) |>
        add_format()[['addBodyStyle']]('left', n_case, 1, sheet) |>
        add_format()[['addBodyStyle']]('right', n_case, 2:n_col, sheet)

      setColWidths(wb, sheet, cols = 1, widths = 16)
      setColWidths(wb, sheet, cols = 2:n_col, widths = 7)

      # add flag color and link
      wb <- add_format()[['colorFlags']](
          wb,
          c(9, 10, 12, 13),
          c('>.5', '>4', '<.05', '=1'),
          sheet,
          n_case
        ) |>
        add_format()[['colorFlags']](
          c(9, 10),
          c('<-.5', '<-4'),
          sheet,
          n_case
        )

      writeFormula(wb, sheet, startRow = 2, startCol = 'O',
                   x = '=HYPERLINK(N$2, "Report")')
      writeFormula(wb, sheet, startRow = 3, startCol = 'O',
                   x = '=HYPERLINK(N$3, "Process")')

      ## Insert images
      img <- file.path(folder, file_name, paste0(sheet, '_', file_name, '.png'))
      insertImage(wb, sheet, img, startRow = 1, startCol = 'P', width = 5.5, height = 10)

      setColWidths(wb, sheet, cols = 'N', widths = 12)
      setColWidths(wb, sheet, cols = 'O', widths = 12)

      # pageBreak(wb, sheet, i = 30, type = "row")
    }
    saveWorkbook(wb, file, overwrite = TRUE)

    writeLines(c(
      paste0('\n\nFiles and plots combined to:'),
      paste0('\tDIF flags:\t', file),
      # paste0('\tDIF plots:\t', out_dif),
      if (folder == 'DIF') paste0('\tFacility plots:\t', out_facil)))
  }

  # #############################################
  # ################ itn files ##################
  # #############################################

  if (sheetNm == 1){
    names(ex_ls) <- tests

    if (prefix=='itn'){ # add summary and flag sheets
      itnSums <- map(ex_ls, ~dplyr::filter(.x, Priority %in% 1:4))
      Flagged <- itnSums[lengths(itnSums) > 0L] |>
        reduce(bind_rows) |>
        select(-21)
      # move combined files into a folder named prefix before saving
      move_into_folder(folder, prefix)

      ls_save <- list(
          Note=itn_comment(),
          Flagged=Flagged |>
            mutate(ICC=paste0('=HYPERLINK(', Test, '!U$2, "ICC")'))
        ) |>
        append(
          map(ex_ls, ~mutate(.x, ICC='=HYPERLINK(U$2, "ICC")'))
        )

      # ###### use openxlsx to add hyperlink, color, format #######

      # add note sheet
      wb <- createWorkbook()
      addWorksheet(wb, names(ls_save)[[1]])
      writeData(wb, sheet = names(ls_save)[[1]], x = ls_save[[1]])

      # add flagged and test sheets
      for (i in 2:length(ls_save)){
        sheet <- names(ls_save)[[i]]
        n_case <- nrow(ls_save[[i]])+1
        n_col <- ifelse(i==2, ncol(ls_save[[i]]), ncol(ls_save[[i]])-1)
        addWorksheet(wb, sheet)
        writeData(wb, sheet = sheet, x = ls_save[[i]])

        # header, body style
        wb <- add_format()[['addHeaderStyle']](wb, n_col, sheet) |>
          add_format()[['addBodyStyle']]('left', n_case, c(4, 20), sheet) |>
          add_format()[['addBodyStyle']]('right', n_case, 7:8, sheet) |>
          add_format()[['addBodyStyle']]('center', n_case, setdiff(1:20, c(4, 20, 7:8)), sheet)

        # add flag color and link
        wb <- add_format()[['colorFlags_link']](wb, i)
      }

      saveWorkbook(wb, file, overwrite = TRUE)

    } else {
      move_into_folder(folder, prefix)
      ex_ls |>
        writexl::write_xlsx(file)
    }

    writeLines(paste0('Files combined to: ', file))
  }

}
