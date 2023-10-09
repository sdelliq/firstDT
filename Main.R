source("Library.R")
source("Functions.R")

file <- 'Data/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx.xlsx'
DT <- read_doc_and_save_df(file)

#Create a DF for the each sheet
NDG.Original <- DT$NDG
LOANS.Original <- DT$LOANS

#Create the NDG dataframe, with the data ready to be dealt with 
source("Ndg.R")

#Creation of the LOANS_FROM_METADATA dataframe according to the metadata we should work with. Leaves the LOANS table filled and clean. Creates an excel report.
source("Loans.R")

#Creation of the Counterparties dataframe 
source("Counterparties.R")

#Creation of the Entities dataframe
source("Entities.R")


#Create link tables 
source("link.loans.counterparties.R")
source("link.counterparties.entities.R")
