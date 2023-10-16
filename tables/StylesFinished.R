wb <- createWorkbook()
addWorksheet(wb, sheetName = "MySheet")

showGridLines(wb, sheet = 1, showGridLines = FALSE)

setColWidths(
  wb,
  sheet=1,
  cols = 1:7,
  widths = "auto"
)

style1_header <- createStyle(
  fgFill = "#FFA500",
  fontSize = 10,
  textDecoration = c("bold"),
  halign = "center",
  wrapText = FALSE
)

style1_rows <- createStyle(
  numFmt = "0.0,,\"M\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
style2_rows <- createStyle(
  fgFill = "#F3EEE0",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
totals_style <- createStyle(
  fgFill = "#E0F3ED",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
thousands_rows <- createStyle(
  numFmt = "0,\"k\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
percentage_rows <- createStyle(
  numFmt = "0.00%",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)

writeData(wb, sheet = "MySheet", x = "Gross Book Value Summarized", startCol = 1, startRow = 1)

writeDataTable(wb, 1, x = r.sumGBVoverall , startRow = 2,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

#addStyle(wb, sheet = "MySheet", style = style1_header, rows = 2:nrow(r.sumGBVoverall+1), cols = 1:ncol(r.sumGBVoverall),stack = TRUE)


addStyle(wb, sheet = "MySheet", style = style1_rows, rows = 3:(nrow(r.sumGBVoverall)+1), cols = 1:ncol(r.sumGBVoverall))


######################################################################


writeData(wb, sheet = "MySheet", x = "Number Of Borrowers", startCol = 1, startRow = nrow(r.sumGBVoverall)+4)
writeDataTable(wb, 1, x = r.numberOfBorrowersOverall , startRow = 6,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")


writeData(wb, sheet = "MySheet", x = "Borrowers With / Without Guarantors By Loans", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall)+ 7)
writeDataTable(wb, 1, x = r.borrowersByLoans.W.and.WO.guarantors , startRow = 10,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 11, cols = 1:ncol(r.borrowersByLoans.W.and.WO.guarantors),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(11,12,13), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(11,12,13), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "MySheet", x = "Borrowers By Type Of Loans", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + 10)
writeDataTable(wb, 1, x = r.borrowersBy.TypeOfLoans , startRow = 16,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 17, cols = 1:ncol(r.borrowersBy.TypeOfLoans),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(17,18,19,20,21), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(17,18,19,20,21), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "MySheet", x = "Borrowers By Loan Status And GBV clusters", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + 13)
writeDataTable(wb, 1, x = r.borrowersBy.StatusOfLoans , startRow = 23,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 24, cols = 1:ncol(r.borrowersBy.StatusOfLoans),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(24,25,26,27,28,29,30,31), cols = c(4) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(24,25,26,27,28,29,30,31), cols = c(6,7) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "MySheet", x = "Borrowers By Gross Book Value Clusters", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + nrow(r.borrowersBy.StatusOfLoans) + 16)
writeDataTable(wb, 1, x = r.borrowersBy.GBVclusters , startRow = 35,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 36, cols = 1:ncol(r.borrowersBy.GBVclusters),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = percentage_rows, rows = c(36,37,38,39), cols = c(3) ,stack = TRUE,gridExpand = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(36,37,38,39), cols = c(5,6) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "MySheet", x = "Borrowers By Area", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + nrow(r.borrowersBy.StatusOfLoans) + nrow(r.borrowersBy.GBVclusters) + 20)
writeDataTable(wb, 1, x = r.borrowersBy.Area , startRow = 43,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 44, cols = 1:ncol(r.borrowersBy.Area),stack = TRUE)

writeData(wb, sheet = "MySheet", x = "Cross Table GBV with/without Guarantors", startCol = 1, startRow = nrow(r.sumGBVoverall)+nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + nrow(r.borrowersBy.StatusOfLoans) + nrow(r.borrowersBy.GBVclusters) + nrow(r.borrowersBy.Area) + 23)
writeDataTable(wb, 1, x = pivot_table , startRow = 48,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

addStyle(wb, sheet = "MySheet", style = totals_style, rows = 49, cols = 1:ncol(pivot_table),stack = TRUE)

addStyle(wb, sheet = "MySheet", style = thousands_rows, rows = c(49,50,51), cols = c(2,3,4) ,stack = TRUE,gridExpand = TRUE)


print(plot)
insertPlot(wb, 1, width = 5, height = 3.5, startRow = 7, startCol = 10, fileType = "png", units = "in")

saveWorkbook(wb, file = "multiple_tables_with_custom_styles.xlsx")
