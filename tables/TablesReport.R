# I would like you to create one excel sheet for tables, and if you can create graphs later to illustrate some interesting findings.
# 
# # Tables:
# -number of borrowers overall
# -sum gbv overall
# -number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
# -number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan
# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
# -number and ratio of borrowers, number of loans, by area
# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors

r.numberOfBorrowersOverall <- NDG %>% summarise("number of borrowers" = n_distinct(borrower.name))
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV sum" = sum(gbv.original))
r.sumGBVoverall <- format(r.sumGBVoverall, big.mark = ".", decimal.mark = ",")




excel_file_path <- "report/reportTables.xlsx"
wb <- createWorkbook()
writeWB(wb, "Tables Report", )
saveWorkbook(wb, file = excel_file_path)