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

###-----------------------------------------------------------------------###
#-----             c) Code                                              -----         
###-----------------------------------------------------------------------###


#Create a DF for the NDG sheet
NDGP <- DT$NDG
LOANSP <- DT$LOANS

# Create NDG_DEP and LOANS_DEP according to their functional dependencies
new_data_frames <- dependent.Tables(LOANS, 0.45)
NDG_N <- as.data.frame(new_data_frames[1])
colnames(NDG_N) <- delete_word_from_col(colnames(NDG_N),"NDG.")
NDG_N <- NDG_N %>% distinct() 
LOANSP <- as.data.frame(new_data_frames[2])
colnames(LOANSP) <- delete_word_from_col(colnames(LOANSP),"ID.Loans.")


#Turn - into NAs
LOANSP <- apply(LOANSP, 2, function(col) replace(col, col == "-", NA))
LOANSP <- as.data.frame(LOANSP)

#See which columns have 80% + NA values inside return them as possible columns to drop and then drop them on personal prefernce
PossibleColsToDel <- columns_to_delete(NDG_N,threshold = 0.8)
print(PossibleColsToDel)
NDG_N <- NDG_N %>% select(-Residual.Position,-Ammisione.al.passivo.privilegiati,-Ammisione.al.passivo.chirografari,-GBV.Expenses)

#Convert Database_Date and Default_Date as Dates
#This is another way to do it: LOANSP[4:5] <- lapply(LOANSP[4:5],convertToDate)  
NDG_N <- NDG_N %>% mutate(across(c(3:4), convertToDate))

#Convert numeric columns as such
LOANSP <- LOANSP %>% mutate(across(c(5:7), as.numeric))

#Convert integer columns as such
LOANSP <- LOANSP %>% mutate_at(vars(NDG, Asset.Link, Guarantee.Amount), as.integer)
NDG_N <- NDG_N %>% mutate_at(vars(NDG, Date.of.last.procedure), as.integer)

# Take the Type.of.Mortgage column and split when in the same row there are values separated by +
LOANSP <- divide_column_by_character(LOANSP, "Type.of.Mortgage", "\\+ ")

# Call the function to split the values 
NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures.CODE", "\\+ ")
NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures", "/ ")

#Create the Judicial DF with NDG as FK and a generated PK
Judicial <- NDG_N %>% select(NDG, Court, Judicial.Procedures, Judicial.Procedures.CODE, Date.of.last.procedure)
Judicial <- Judicial %>% tibble::rowid_to_column("Judicial.ID")

#Deletes the columns that went into the Judicial dataframe
NDG_N <- NDG_N %>% select(- c(Court, Judicial.Procedures, Judicial.Procedures.CODE, Date.of.last.procedure)) 
NDG_N <- NDG_N %>%distinct()

#Create the Bankruptcy Table and delete its columns from the NDG table
Bankruptcy <- NDG_N %>% select(NDG, Bankruptcy.Proceedings, Bankruptcy.Code.Proceedings, Bankruptcy.Proceedings.NOTE)
Bankruptcy <- na.omit(Bankruptcy)
NDG_N <- NDG_N %>% select(- c(Bankruptcy.Proceedings, Bankruptcy.Code.Proceedings, Bankruptcy.Proceedings.NOTE)) 

#Join the table we had on the other sheet to the NDG
NDG_N <- NDG_N %>% select(-Group)
NDG_N <- NDG_N %>% inner_join(Group_Of_Borrowers, by="NDG")

#Create Guarantors without NA and separating the rows with multiple values. It then adds the 
Guarantors <- LOANSP %>% select (NDG, Guarantors.Name, TAX.CODE.for.Guarantors) %>% distinct()
LOANSP <- LOANSP %>% select(-colnames(Guarantors))
Guarantors <- na.omit(Guarantors)
Guarantors <- divide_column_by_character(Guarantors, c(Guarantors.Name, TAX.CODE.for.Guarantors), ",")
Guarantors <- Guarantors %>% distinct() 

Guarantors <- Guarantors %>%
  mutate(Guarantor.ID = group_indices(., Guarantors.Name, TAX.CODE.for.Guarantors))

#Creates a linking table
Link_Guarantors_NDG <- Guarantors %>% select(Guarantor.ID, NDG)
Guarantors <- Guarantors %>% select(-NDG) %>% distinct()







