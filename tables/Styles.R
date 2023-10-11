

r.numberOfBorrowersOverall <- NDG %>% summarise("number of borrowers" = n_distinct(borrower.name))
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV sum" = sum(gbv.original))



r.sumGBVoverall$`GBV sum` <- as.numeric(r.sumGBVoverall$`GBV sum`)


# Create a function to write a data frame with styles to a sheet
writeTableWithStyles <- function(wb, sheet, data, header_style, first_row_style, startRow) {
  writeData(wb, sheet = sheet, x = data, startCol = 1, startRow = startRow, colNames = TRUE)
  
  num_cols <- ncol(data)
  
  for (col in 1:num_cols) {
    addStyle(wb, sheet = sheet, rows = startRow, cols = col, style = header_style)
  }
  
  for (col in 1:num_cols) {
    addStyle(wb, sheet = sheet, rows = startRow + 1, cols = col, style = first_row_style)
  }
  
  setColWidths(wb, sheet = sheet, cols = 1:num_cols, widths = "auto")
}

# Define the file path
file_path <- "TryingTables.xlsx"

# Create the workbook and add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "Report")

# Create a style object for the header (for the first data frame)
header_style1 <- createStyle(
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

# Create a style object for the header (for the second data frame)
header_style2 <- createStyle(
  fgFill = "#FFA500",
  fontSize = 10,
  halign = "center",
  valign = "center",
  textDecoration = c("bold"),
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

# Write the first data frame to the Excel sheet with styles
writeTableWithStyles(wb, "Report", r.sumGBVoverall, header_style1, first_row_style1, startRow = 1)

# Write the second data frame to the same sheet with different styles
writeTableWithStyles(wb, "Report", r.numberOfBorrowersOverall, header_style2, first_row_style2, startRow = nrow(r.sumGBVoverall) + 3)

# Save the workbook as an Excel file
saveWorkbook(wb, file = file_path)


