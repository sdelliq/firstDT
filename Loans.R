#Creation of the loans table according to the metadata we should work with

#Importing the libraries and functions
source("Library.R")
source("Functions.R")

file <- 'Data/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx.xlsx'
DT <- read_doc_and_save_df(file)

LOANS_Raw <- DT$LOANS
#DEALING WITH THE LOANS DATAFRAME
LOANS <- LOANS_Raw
#Delete first two rows and renames the columns 
colnames(LOANS) <- LOANS[2, ] 
LOANS <- LOANS[-1:-2,] 
rownames(LOANS) <- 1:nrow(LOANS)

#Cleans the column names and gives all of them to the same name convention
colnames(LOANS) <- clean_column_names(colnames(LOANS))

#Rename columns to make them match the name they're supposed to have (seen on the Metadata file)
LOANS <- LOANS %>% rename("id.bor" = ndg, "type" = type.of.credit, "gbv.original" = total.gbv, "gbv.residual" = residual.position, "principal" = gbv.capital, "interest" = gbv.interest, "expenses" = gbv.expenses, "date.status" = default.date)


#Take the UTPs from the date
LOANS$status <- LOANS$date.status
LOANS <-  LOANS %>% relocate(status, .after = date.status)
LOANS$status <- replace(LOANS$status, LOANS$status!="UTP", "BAD")

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
LOANS_FROM_METADATA <- LOANS %>% select (id.bor, id.group, originator, ptf, cluster.ptf, type, status, gbv.original, gbv.residual, principal, interest, penalties, expenses, date.origination, date.status, date.last.act, flag.imputed)
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(id.bor, id.group, originator, ptf, cluster.ptf, type, status), as.character) )
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(gbv.original, gbv.residual, principal, interest, penalties, expenses), as.numeric))
LOANS_FROM_METADATA <- LOANS_FROM_METADATA %>% mutate(across(c(date.origination, date.status, date.last.act, flag.imputed), convertToDate))
LOANS_FROM_METADATA$gbv.residual <- LOANS_FROM_METADATA$gbv.original
