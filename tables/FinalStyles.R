wb <- createWorkbook()
addWorksheet(wb, sheetName = "MySheet")

showGridLines(wb, sheet = 1, showGridLines = FALSE)

style1_header <- createStyle(
  fgFill = "#FFA500",
  fontSize = 10,
  textDecoration = c("bold"),
  halign = "center"
)

style1_rows <- createStyle(
  numFmt = "0.0,,\"M\"",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)
style2_rows <- createStyle(
  fgFill = "#F3EEE0",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)
totals_style <- createStyle(
  fgFill = "#F2E0DC",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)
thousands_rows <- createStyle(
  numFmt = "0,\"k\"",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)
percentage_rows <- createStyle(
  numFmt = "0.00%",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)

writeData(wb, sheet = "MySheet", x = "Gross Book Value Summarized", startCol = 1, startRow = 1)

writeDataTable(wb, 1, x = r.sumGBVoverall , startRow = 2,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

#addStyle(wb, sheet = "MySheet", style = style1_header, rows = 2:nrow(r.sumGBVoverall+1), cols = 1:ncol(r.sumGBVoverall),stack = TRUE)


addStyle(wb, sheet = "MySheet", style = style1_rows, rows = 3:(nrow(r.sumGBVoverall)+1), cols = 1:ncol(r.sumGBVoverall))

saveWorkbook(wb, file = "multiple_tables_with_custom_styles.xlsx")

######################################################################


writeData(wb, sheet = "MySheet", x = "Number Of Borrowers", startCol = 1, startRow = nrow(r.sumGBVoverall)+4,headerStyle = style1_header)
writeDataTable(wb, 1, x = r.numberOfBorrowersOverall , startRow = 6,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

#addStyle(wb, sheet = "MySheet", style = style2_rows, rows = 2:(nrow(r.numberOfBorrowersOverall)+1), cols = 1:ncol(r.numberOfBorrowersOverall),stack = TRUE)
addStyle(wb, sheet = "MySheet", style = style2_rows, rows = 7, cols = 1:ncol(r.numberOfBorrowersOverall),stack = TRUE)


writeData(wb, sheet = "MySheet", x = r.borrowersByLoans.W.and.WO.guarantors, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+5,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 8, cols = 1:ncol(r.borrowersByLoans.W.and.WO.guarantors),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(9,10), cols = 1:ncol(r.borrowersByLoans.W.and.WO.guarantors),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(8,9,10), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(8,9,10), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "MySheet", x = r.borrowersBy.TypeOfLoans, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+nrow(r.borrowersByLoans.W.and.WO.guarantors)+7,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 13, cols = 1:ncol(r.borrowersBy.TypeOfLoans),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(14,15,16,17), cols = 1:ncol(r.borrowersBy.TypeOfLoans),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(13,14,15,16,17), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(13,14,15,16,17), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)

########

writeData(wb, sheet = "MySheet", x = r.borrowersBy.StatusOfLoans, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+nrow(r.borrowersByLoans.W.and.WO.guarantors)+nrow(r.borrowersBy.TypeOfLoans)+ 9,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 20, cols = 1:ncol(r.borrowersBy.StatusOfLoans),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(21,22,23,24,25,26,27), cols = 1:ncol(r.borrowersBy.StatusOfLoans),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(20,21,22,23,24,25,26,27), cols = c(4) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(20,21,22,23,24,25,26,27), cols = c(6,7) ,stack = TRUE,gridExpand = TRUE)


########

writeData(wb, sheet = "MySheet", x = r.borrowersBy.GBVclusters, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+nrow(r.borrowersByLoans.W.and.WO.guarantors)+nrow(r.borrowersBy.TypeOfLoans)+ nrow(r.borrowersBy.StatusOfLoans) +12,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 31, cols = 1:ncol(r.borrowersBy.GBVclusters),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(32,33,34), cols = 1:ncol(r.borrowersBy.GBVclusters),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(31,32,33,34), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(31,32,33,34), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)

#######

writeData(wb, sheet = "MySheet", x = r.borrowersBy.Area, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+nrow(r.borrowersByLoans.W.and.WO.guarantors)+nrow(r.borrowersBy.TypeOfLoans)+ nrow(r.borrowersBy.StatusOfLoans) + nrow(r.borrowersBy.GBVclusters) +14,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 37, cols = 1:ncol(r.borrowersBy.Area),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(38), cols = 1:ncol(r.borrowersBy.Area),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(37,38), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

########

writeData(wb, sheet = "MySheet", x = pivot_table, startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+nrow(r.borrowersByLoans.W.and.WO.guarantors)+nrow(r.borrowersBy.TypeOfLoans)+ nrow(r.borrowersBy.StatusOfLoans) + nrow(r.borrowersBy.GBVclusters) + nrow(r.borrowersBy.Area)+ 16,headerStyle = style1_header)

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 41, cols = 1:ncol(pivot_table),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = style2_rows, rows = c(42,43), cols = 1:ncol(r.borrowersBy.Area),stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(41,42,43), cols = c(2,3,4) ,stack = TRUE,gridExpand = TRUE)

saveWorkbook(wb, file = "multiple_tables_with_custom_styles.xlsx")
