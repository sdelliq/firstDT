#Creation of the LOANS_FROM_METADATA table according to the metadata we should work with. Leaves the LOANS table filled and clean. Creates an excel report.


#DEALING WITH THE LOANS DATAFRAME
LOANS <- LOANS.Original
#Delete first two rows and renames the columns 
colnames(LOANS) <- LOANS[2, ] 
LOANS <- LOANS[-1:-2,] 
rownames(LOANS) <- 1:nrow(LOANS)

#Cleans the column names and gives all of them to the same name convention
colnames(LOANS) <- clean_column_names(colnames(LOANS))

#Puts the whole df in lowercase
LOANS <- LOANS %>% mutate_all(tolower)

#Rename columns to make them match the name they're supposed to have (seen on the Metadata file)
LOANS <- LOANS %>% rename("id.bor" = ndg, "type" = type.of.credit, "gbv.original" = total.gbv, "gbv.residual" = residual.position, "principal" = gbv.capital, "interest" = gbv.interest, "expenses" = gbv.expenses, "date.status" = default.date)


#Take the UTPs from the date
LOANS$status <- LOANS$date.status
LOANS <-  LOANS %>% relocate(status, .after = date.status)
LOANS$status <- replace(LOANS$status, LOANS$status!="utp", "bad")

#Creates columns filled with NAs
LOANS <- LOANS %>%
  mutate(
    id.group = NA,
    originator = NA,
    ptf = NA,
    cluster.ptf = NA,
    penalties = NA,
    date.origination = NA,
    date.last.act = NA,
    flag.imputed = NA
  )

#Creates the LOANS_FROM_METADATA dataframe with the columns it should have and its corresponding types
LOANS_FROM_METADATA <- LOANS %>% select (id.loans, id.bor, id.group, originator, ptf, cluster.ptf, type, status, gbv.original, gbv.residual, principal, interest, penalties, expenses, date.origination, date.status, date.last.act, flag.imputed)
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(id.bor, id.group, originator, ptf, cluster.ptf, type, status), as.character) )
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(gbv.original, gbv.residual, principal, interest, penalties, expenses), as.numeric))
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(date.origination, date.status, date.last.act, flag.imputed), convertToDate))
LOANS_FROM_METADATA$gbv.residual <- LOANS_FROM_METADATA$gbv.original

#type: Other|Credit Cards|Bank Accounts|Personal Loans|Mortgages|Mortgages (Fondiario)
LOANS_FROM_METADATA$type <- sub("n\\..*$", "", LOANS_FROM_METADATA$type)
LOANS_FROM_METADATA$type <- str_trim(LOANS_FROM_METADATA$type, side = "right")

# Renames the type according to what it should be - personal opinion
LOANS_FROM_METADATA$type <- LOANS_FROM_METADATA$type %>% lapply(function(x){
  if(x == "conto corrente chiro"){
    x <- 'bank accounts'
  }
  else if(x == "mutuo ipotecario"){
    x <- 'mortgages'
  }
  else if(x == "credito di firma"){
    x <- 'personal loans'
  }
  else if(x == "mutuo chiro"){
    x <- 'personal loans'
  }
  else if(x == "mutuo chirografario"){
    x <- 'personal loans'
  }
  else{
    print("you're missing something")
  }
})



report.unique.types <- LOANS_FROM_METADATA %>% group_by(type) %>% summarise(count=n())
colnames(report.unique.types) <- c("type.of.loan", "amount.of.loans")

report.gbv <- LOANS_FROM_METADATA %>% select (gbv.original, gbv.residual, principal, interest)
report.gbv <- apply(report.gbv, 2, function(x){
  c(somma = sum(x), minimo = min(x), massimo = max(x))
} )
report.gbv <- as.data.frame(t(report.gbv))
report.gbv <- format(report.gbv, big.mark = ".", decimal.mark = ",")
report.gbv <- cbind(" " = row.names(report.gbv), report.gbv)

excel_file_path <- "report/report.xlsx"
wb <- createWorkbook()
# Add data frames to the workbook
writeWB(wb, "Types Of Loans Analysis", report.unique.types)
writeWB(wb, "GBV Analysis", report.gbv)

saveWorkbook(wb, file = excel_file_path)










