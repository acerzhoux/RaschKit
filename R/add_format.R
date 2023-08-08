#' add_format
#'
#' This function returns a list of functions to add format such as header and
#' body style to Excel sheet.
#'
#' @export

add_format <- function(){
  # functions to add formats, hyperlink
  colorFlags_link <- function(wb, i, ls_save){
    cols <- c(10, 11, 13, 14)
    colWide <- ifelse(i==2, 20, 21)
    rules <- c('<10', '<.11', '>1.2', '>1.1')
    sheet <- names(ls_save)[[i]]
    n_case <- nrow(ls_save[[i]])+1

    posStyle <- createStyle(
      halign = "left",
      bgFill = "#FFC7CE",
      wrapText=TRUE
    )
    for (j in 1:4){
      conditionalFormatting(
        wb=wb,
        sheet=sheet,
        cols=cols[[j]],
        rows=2:n_case,
        rule=rules[[j]],
        type = "expression",
        style = posStyle
      )
    }
    setColWidths(wb, sheet, cols = 1, widths = 14)
    setColWidths(wb, sheet, cols = 2:3, widths = 8)
    setColWidths(wb, sheet, cols = 4, widths = 15)
    setColWidths(wb, sheet, cols = 5:6, widths = 6)
    setColWidths(wb, sheet, cols = 7:19, widths = 8)
    setColWidths(wb, sheet, cols = 20, widths = 90)

    if (colWide==21) {
      setColWidths(wb, sheet, cols = colWide, widths = 10)
      setColWidths(wb, sheet, cols = colWide+1, widths = 10)
    }

    # add hyperlink
    for (k in 2:n_case){
      writeFormula(wb, sheet,
                   startRow = k, startCol = 3,
                   x = ls_save[[i]]$ICC[k-1]
      )
    }

    return(wb)
  }

  addHeaderStyle <- function(wb, n_col, sheet, size=8){
    headerStyle <- createStyle(
      fontSize = size,
      textDecoration = 'bold',
      halign = "center",
      valign = 'bottom',
      fgFill='blue',
      fontColour='white',
      fontName='Arial',
      border = "TopBottomLeftRight",
      wrapText = TRUE
    )
    addStyle(
      wb,
      sheet = sheet,
      headerStyle,
      rows = 1,
      cols = 1:n_col,
      gridExpand = FALSE
    )
    return(wb)
  }

  addBodyStyle <- function(wb, halign, n_case, cols, sheet, size=8){
    bodyStyle <- createStyle(
      fontSize = size,
      halign = halign,
      fontName='Arial',
      border = "TopBottomLeftRight",
      wrapText = TRUE
    )
    addStyle(
      wb,
      sheet = sheet,
      bodyStyle,
      rows = 2:n_case,
      cols = cols,
      gridExpand = TRUE
    )
    return(wb)
  }

  colorFlags <- function(wb, cols, rules, sheet, n_case){
    posStyle <- createStyle(
      halign = "left",
      bgFill = "#FFC7CE",
      wrapText=TRUE
    )
    for (j in 1:length(cols)){
      conditionalFormatting(
        wb=wb,
        sheet=sheet,
        cols=cols[[j]],
        rows=2:n_case,
        rule=rules[[j]],
        type = "expression",
        style = posStyle
      )
    }
    return(wb)
  }

  colorFlagsDifPoly <- function(wb, sheet, n_case, nCat, df){
    redStyle <- createStyle(
      halign = "right",
      bgFill = "#FFC7CE",
      wrapText=TRUE
    )
    greenStyle <- createStyle(
      halign = "right",
      bgFill = "#00FF00",
      wrapText=TRUE
    )

    for (j in 0:(nCat-1)){
      nCol <- 2*j+3
      for (k in 2:n_case){
        if (!is.na(df[k-1, nCol+1])){
          conditionalFormatting(
            wb=wb,
            sheet=sheet,
            cols=nCol,
            rows=k,
            rule=paste0(
              'AND($', int2col(nCol), k, '>0, EXACT($', int2col(nCol+1), k, ', \"*\"))'
            ),
            type = "expression",
            style = redStyle
          )
          conditionalFormatting(
            wb=wb,
            sheet=sheet,
            cols=nCol,
            rows=k,
            rule=paste0(
              'AND($', int2col(nCol), k, '<0, EXACT($', int2col(nCol+1), k, ', \"*\"))'
            ),
            type = "expression",
            style = greenStyle
          )
        }
      }
    }

    return(wb)
  }

  # itn
  itn <- function(ls_save, file, folder, prefix){
    # add note sheet
    wb <- createWorkbook()
    for (i in 1:2) {
      addWorksheet(wb, names(ls_save)[[i]])
      writeData(wb, sheet = names(ls_save)[[i]], x = ls_save[[i]])
    }

    # add flagged and test sheets
    for (i in 3:length(ls_save)){
      sheet <- names(ls_save)[[i]]
      n_case <- nrow(ls_save[[i]])+1
      n_col <- ifelse(i==3, ncol(ls_save[[i]]), ncol(ls_save[[i]])-1)
      addWorksheet(wb, sheet)
      writeData(wb, sheet = sheet, x = ls_save[[i]])

      # header, body style
      wb <- addHeaderStyle(wb, n_col, sheet) |>
        addBodyStyle('left', n_case, c(4, 20), sheet) |>
        addBodyStyle('right', n_case, 7:8, sheet) |>
        addBodyStyle('center', n_case, setdiff(1:20, c(4, 20, 7:8)), sheet)

      # add flag color and link
      wb <- colorFlags_link(wb, i, ls_save)

      if (i > 3) {
        shwVec <- c('ICC', 'ipMap', 'Converge', 'Freq QA')
        for (j in 2:5) {
          writeFormula(
            wb, sheet,
            startRow = j,
            startCol = n_col+2,
            x = paste0('=HYPERLINK(', LETTERS[[n_col+1]], '$', j, ', \"', shwVec[[j-1]], '\")')
          )
        }
      }
    }
    saveWorkbook(wb, file, overwrite = TRUE)
  }

  # equating
  equate <- function(ls_save, folder, file, flagVec){
    # add note sheet
    wb <- createWorkbook()
    sheet <- names(ls_save)[[1]]
    n_case <- nrow(ls_save[[1]])+1
    n_col <- ncol(ls_save[[1]])

    addWorksheet(wb, sheet)
    names(ls_save[[1]]) <- gsub("_", ' ', names(ls_save[[1]]))
    writeData(wb, sheet = sheet, x = ls_save[[1]])

    # header, body style
    wb <- addHeaderStyle(wb, n_col, sheet) |>
      addBodyStyle('left', n_case, 1, sheet) |>
      addBodyStyle('right', n_case, 2:n_col, sheet)

    setColWidths(wb, sheet, cols = 1, widths = 12)
    setColWidths(wb, sheet, cols = 2:n_col, widths = 8)

    # add flagged and test sheets
    for (i in 2:length(ls_save)){
      sheet <- names(ls_save)[[i]]
      n_case <- nrow(ls_save[[i]])+1
      n_col <- ncol(ls_save[[i]])

      colsFlag1 <- n_col-4
      colsFlag <- colsFlag1 + c(0, 1, 3, 4)

      addWorksheet(wb, sheet)
      writeData(
        wb,
        sheet = sheet,
        x = ls_save[[i]] |>
          dplyr::mutate(
            `Files`=c(
              file.path(
                gsub('equating/', '', gsub('DIF/', '', folder)),
                paste0(sheet, '_process.xlsx')
              ),
              rep(NA, n_case-1-1)
            )
          )
      )

      # header, body style
      wb <- addHeaderStyle(wb, n_col, sheet) |>
        addBodyStyle('left', n_case, 1, sheet) |>
        addBodyStyle('right', n_case, 2:n_col, sheet)

      setColWidths(wb, sheet, cols = 1, widths = 15)
      setColWidths(wb, sheet, cols = 2:n_col, widths = 7)

      # add flag color and link
      wb <- colorFlags(
          wb,
          colsFlag,
          c(paste0('>', flagVec[[1]][i-1]), paste0('>', flagVec[[2]][i-1]), '<.05', '=1'),
          sheet,
          n_case
        ) |>
        colorFlags(
          colsFlag[1:2],
          c(paste0('<-', flagVec[[1]][i-1]), paste0('<-', flagVec[[2]][i-1])),
          sheet,
          n_case
        )

      writeFormula(
        wb, sheet,
        startRow = 2,
        startCol = n_col+2,
        x = paste0('=HYPERLINK(', LETTERS[[n_col+1]], '$2, "Process")')
      )

      ## Insert images
      insertImage(
        wb, sheet,
        file.path(folder, paste0(sheet, '_delta.png')),
        startRow = 1, startCol = n_col+3,
        width = 5.5, height = 10
      )
      if (colsFlag1 > 9 & identical(grep('step', sheet), integer(0))) {
        insertImage(
          wb, sheet,
          file.path(folder, paste0(sheet, '_facilDiscrFitw.png')),
          startRow = 1, startCol = n_col+10,
          width = 5.5, height = 10
        )
      }

      setColWidths(wb, sheet, cols = n_col+1, widths = 10)
      setColWidths(wb, sheet, cols = n_col+2, widths = 10)

      # pageBreak(wb, sheet, i = 30, type = "row")
    }
    saveWorkbook(wb, file, overwrite = TRUE)
  }

  # DIF polytomous variable
  DIFPoly <- function(ls_save, folder, file){
    # add note sheet
    wb <- createWorkbook()
    nCat <- (ncol(ls_save[[2]])-2)/2

    # add flagged and test sheets
    for (i in seq_along(ls_save)){
      sheet <- names(ls_save)[[i]]
      n_case <- nrow(ls_save[[i]])+1
      n_col <- ncol(ls_save[[i]])
      df <- ls_save[[i]]

      addWorksheet(wb, sheet)
      if (i==1) {
        writeData(
          wb,
          sheet = sheet,
          x = ls_save[[i]]
        )
      } else {
        writeData(
          wb,
          sheet = sheet,
          x = ls_save[[i]] |>
            dplyr::mutate(
              `Files`=c(
                file.path(folder, paste0(sheet, '_process.xlsx')),
                rep(NA, n_case-1-1)
              )
            )
        )
        writeFormula(
          wb, sheet,
          startRow = 2,
          startCol = n_col+2,
          x = paste0('=HYPERLINK(', int2col(n_col+1), '$2, "Process")')
        )
        insertImage(
          wb, sheet,
          file.path(folder, paste0(sheet, '_delta.png')),
          startRow = 1, startCol = n_col+3,
          width = 34, height = 60, "cm"
        )
      }

      # header, body style
      wb <- addHeaderStyle(wb, n_col, sheet) |>
        addBodyStyle('left', n_case, 1:2, sheet) |>
        addBodyStyle('right', n_case, 3:n_col, sheet)

      # set colunm width
      setColWidths(wb, sheet, cols = 1, widths = 10)
      setColWidths(wb, sheet, cols = 2, widths = 15)
      for (j in 0:(nCat-1)){
        nCol <- 2*j+3
        setColWidths(wb, sheet, cols = nCol, widths = 7)
        setColWidths(wb, sheet, cols = nCol+1, widths = 7)
        addStyle(
          wb,
          sheet = sheet,
          createStyle(halign = 'left', border = "TopBottomLeftRight"),
          rows = 2:n_case,
          cols = nCol+1,
          gridExpand = TRUE
        )
      }

      # add flag color
      wb <- colorFlagsDifPoly(wb, sheet, n_case, nCat, df)

      if (i != 1){
        setColWidths(wb, sheet, cols = n_col+1, widths = 10)
        setColWidths(wb, sheet, cols = n_col+2, widths = 10)
      }

    }
    saveWorkbook(wb, file, overwrite = TRUE)
  }

  # DIF polytomous: Facet model
  DIFFacet <- function(ls_save, grpFile, file){
    # add note sheet
    wb <- createWorkbook()

    # add flagged and test sheets
    for (i in 1:length(ls_save)){
      sheet <- names(ls_save)[[i]]
      n_case <- nrow(ls_save[[i]])+1
      n_col <- ncol(ls_save[[i]])
      df <- ls_save[[i]]

      if (i==1) {
        ls_save[[i]] <- ls_save[[i]] |>
          dplyr::mutate(
            `Files`=c(
              grpFile,
              rep(NA, n_case-1-1)
            )
          )
      }

      addWorksheet(wb, sheet)
      writeData(
        wb,
        sheet = sheet,
        x = ls_save[[i]]
      )

      if (i==1) {
        writeFormula(
          wb, sheet,
          startRow = 2,
          startCol = n_col+2,
          x = paste0('=HYPERLINK(', LETTERS[[n_col+1]], '$2, "Plot")')
        )

        setColWidths(wb, sheet, cols = n_col+1, widths = 10)
        setColWidths(wb, sheet, cols = n_col+2, widths = 10)
      }

      # header, body style
      edge <- ifelse(i==1, n_col, n_col-2)
      wb <- addHeaderStyle(wb, edge, sheet) |>
        addBodyStyle('center', n_case, 1, sheet) |>
        addBodyStyle('left', n_case, 2, sheet) |>
        addBodyStyle('center', n_case, 3, sheet) |>
        addBodyStyle('right', n_case, 4:9, sheet) |>
        addBodyStyle('center', n_case, 10:edge, sheet)

      # add flag color and link
      posStyle <- createStyle(
        halign = "left",
        bgFill = "#FFC7CE",
        wrapText=TRUE
      )
      for (j in 10:13){
        if (j==13) {
          rule <- '=1'
        }
        for (k in 2:n_case){
          if (!is.na(df[k-1, j])){
            if (j %in% 10:12) {
              rule <- paste0('EXACT($', LETTERS[[j]], k, ', \"*\")')
            }
            conditionalFormatting(
              wb=wb,
              sheet=sheet,
              cols=j,
              rows=k,
              rule=rule,
              type = "expression",
              style = posStyle
            )
          }
        }
      }
    }
    saveWorkbook(wb, file, overwrite = TRUE)
  }

  # return functions
  list(
    itn=itn,
    equate=equate,
    DIFPoly=DIFPoly,
    DIFFacet=DIFFacet
  )

}
