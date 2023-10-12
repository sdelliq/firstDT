

r.numberOfBorrowersOverall <- NDG %>% summarise("number of borrowers" = n_distinct(borrower.name))
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV sum" = sum(gbv.original))



r.sumGBVoverall$`GBV sum` <- as.numeric(r.sumGBVoverall$`GBV sum`)


# Create a function to write a data frame with styles to a sheet
# Function to write a data frame with styles to a sheet

writeTableWithStyles <- function(wb, sheet, data, header_style, first_row_style, startRow) {
  writeData(wb, sheet = sheet, x = data, startCol = 1, startRow = startRow, colNames = TRUE)
  
  num_cols <- ncol(data)
  num_rows <- nrow(data)
  
  for (col in 1:num_cols) {
    addStyle(wb, sheet = sheet, rows = startRow, cols = col, style = header_style)
  }
  
  for (i in 1:num_rows) {  # Start from row 1 to include the first row but skip the header
    for (col in 1:num_cols) {
      addStyle(wb, sheet = sheet, rows = startRow + i, cols = col, style = first_row_style)
    }
  }
  
  setColWidths(wb, sheet = sheet, cols = 1:num_cols, widths = "auto")
}
# Define the file path
file_path <- "TryingTables.xlsx"

# Create the workbook and add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "Report")

# Create a style object for the header (for the first data frame)
common_header_style <- createStyle(
  fgFill = "#FFA500",
  fontSize = 10,
  halign = "center",
  valign = "center",
  textDecoration = c("bold"),
  fontColour = "black"
)

# Create a style object for the first row (for the first data frame)
first_row_style1 <- createStyle(
  numFmt = "0.0,,\"M\"",
  fgFill = "#F2E0DC",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)


# Create a style object for the first row (for the second data frame)
first_row_style2 <- createStyle(
  fgFill = "#F2E0DC",
  fontSize = 10,
  halign = "center",
  valign = "center",
  fontColour = "black"
)


startRow2 <- nrow(r.sumGBVoverall) + 3
startRow3 <- nrow(r.sumGBVoverall) + nrow(r.numberOfBorrowersOverall) + 6
startRow4 <- nrow(r.sumGBVoverall) + nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + 9
startRow5 <- nrow(r.sumGBVoverall) + nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + 12
startRow6 <- nrow(r.sumGBVoverall) + nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + nrow(r.borrowersBy.StatusOfLoans) + 15
startRow7 <- nrow(r.sumGBVoverall) + nrow(r.numberOfBorrowersOverall) + nrow(r.borrowersByLoans.W.and.WO.guarantors) + nrow(r.borrowersBy.TypeOfLoans) + nrow(r.borrowersBy.StatusOfLoans) + nrow(r.borrowersBy.GBVclusters) + 18


# Write the first data frame to the Excel sheet with styles
writeTableWithStyles(wb, "Report", r.sumGBVoverall, common_header_style, first_row_style1, startRow = 1)

# Write the second data frame to the same sheet with different styles
writeTableWithStyles(wb, "Report", r.numberOfBorrowersOverall, common_header_style, first_row_style2, startRow = nrow(r.sumGBVoverall) + 3)

writeTableWithStyles(wb, "Report", r.borrowersByLoans.W.and.WO.guarantors , common_header_style, first_row_style2, startRow = startRow3)

writeTableWithStyles(wb, "Report", r.borrowersBy.TypeOfLoans , common_header_style, first_row_style2, startRow = startRow4)

writeTableWithStyles(wb, "Report", r.borrowersBy.StatusOfLoans , common_header_style, first_row_style2, startRow = startRow5)

writeTableWithStyles(wb, "Report", r.borrowersBy.GBVclusters , common_header_style, first_row_style2, startRow = startRow6)

writeTableWithStyles(wb, "Report", r.borrowersBy.Area , common_header_style, first_row_style2, startRow = startRow7)

# Save the workbook as an Excel file
saveWorkbook(wb, file = file_path)


