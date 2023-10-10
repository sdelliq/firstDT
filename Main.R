###-----------------------------------------------------------------------###
#-----             a) Environment                                       -----         
###-----------------------------------------------------------------------###

install.packages("renv")
library(renv)
renv::init() 
#renv::install() #Only used when installing the packages for the first time
#renv::update() #Only used if we want to update packages 
renv::snapshot()

###-----------------------------------------------------------------------###
#-----             b) Importing libraries and functions                 -----         
###-----------------------------------------------------------------------###
source("Library.R")
source("Functions.R")

file <- 'Data/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx.xlsx'
DT <- read_doc_and_save_df(file)

#Create a DF for the each sheet
NDG.Original <- DT$NDG
LOANS.Original <- DT$LOANS

###-----------------------------------------------------------------------###
#-----             c) Running the files. In order.                 -----         
###-----------------------------------------------------------------------###


#Create the NDG dataframe, with the data ready to be dealt with 
source("tables/Ndg.R")

#Creation of the LOANS_FROM_METADATA dataframe according to the metadata we should work with. Leaves the LOANS table filled and clean. Creates an excel report.
source("tables/Loans.R")

#Creation of the Counterparties dataframe 
source("tables/Counterparties.R")

#Creation of the Entities dataframe
source("tables/Entities.R")


#Create link tables 
source("tables/link.loans.counterparties.R")
source("tables/link.counterparties.entities.R")
