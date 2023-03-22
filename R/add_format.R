#' add_format
#'
#' This function returns a list of functions to add format such as header and
#' body style to Excel sheet.
#'
#' @export

add_format <- function(){
  # functions to add formats, hyperlink
  colorFlags_link <- function(wb, i){
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
    setColWidths(wb, sheet, cols = 4, widths = 16)
    setColWidths(wb, sheet, cols = 5:6, widths = 6)
    setColWidths(wb, sheet, cols = 7:19, widths = 8)
    setColWidths(wb, sheet, cols = 20, widths = 90)

    if (colWide==21) setColWidths(wb, sheet, cols = colWide, widths = 50)

    # add hyperlink
    for (k in 2:n_case){
      writeFormula(wb, sheet,
                   startRow = k, startCol = 3,
                   x = ls_save[[i]]$ICC[k-1]
      )
    }

    return(wb)
  }

  addHeaderStyle <- function(wb, n_col, sheet){
    headerStyle <- createStyle(
      fontSize = 10,
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

  addBodyStyle <- function(wb, halign, n_case, cols, sheet){
    bodyStyle <- createStyle(
      fontSize = 10,
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

  list(
    colorFlags_link=colorFlags_link,
    addHeaderStyle=addHeaderStyle,
    addBodyStyle=addBodyStyle,
    colorFlags=colorFlags
  )

}
