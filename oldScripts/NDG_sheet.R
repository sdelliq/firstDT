#This is mostly the first code we wrote. The normalization and cleaning made by us without creating functions. 

source("Library.R")
source("Functions.R")

file <- 'Data/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx.xlsx'
DT <- read_doc_and_save_df(file)

#Create a DF for the NDG sheet
NDGP <- DT$NDG
#DEALING WITH THE NDG DATAFRAME
NDG <- NDGP

#Returns the possible primary keys 
possiblePKs(NDG)

#### Separate rows that contain more than 1 value and make them columns
NDG <- NDG %>% 
  separate(BorrowerName,c('BorrowerName1','BorrowerName2','BorrowerName3','BorrowerName4'),sep = '[-,,]') %>% separate(`Tax ID`,c('TaxID1','TaxID2','TaxID3'),sep = '[-,,]')
### Match columns to pivot and drop NAs
NDG <-  NDG %>% 
  pivot_longer(cols = matches('BorrowerN|Tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(BorrowerName|TaxID)(.*)",values_drop_na =  TRUE)

#Delete the column tax_ID (created when pivoting)
NDG <- NDG  %>% select(-taxID)

#Cleaning 
NDG <- NDG %>% rename("Region" = `Borrower's Region`)
NDG <- NDG %>% rename("Name" = BorrowerName)
colnames(NDG) <- clean_column_names(colnames(NDG))
NDG <- NDG %>% mutate_all(~str_trim(., 'left'))
NDG$Group <- replace(NDG$Group, NDG$Group=="-", NA)

#Relocate TaxID column
NDG <- NDG %>% relocate(Tax.ID, .after= NDG)

#Creates the Towns dataframe and only keeps the unique values 
Towns <- NDG %>% select (Town, City, Region) %>% distinct(Town, City, Region)
Towns <- Towns %>% tibble::rowid_to_column("Town.ID")
#Giving the NDG table the ID from Towns
NDG <- NDG %>% inner_join(Towns, by="Town")
NDG <- NDG %>% select (NDG, Tax.ID, Name, Group, Category, Town.ID, Address)

#Creates the Addresses table, keeps unique values, and creates an ID 
Addresses <- NDG %>% select(Address, Town.ID)
Addresses <- Addresses %>% distinct(Address, Town.ID)
Addresses <- Addresses %>% tibble::rowid_to_column("Address.ID")
NDG <- NDG %>% inner_join(Addresses, by = "Address")
NDG <- NDG %>% select (NDG, Tax.ID, Name, Group, Category, Address.ID)

#Creates the Borrower table, keeps unique values, and creates an ID
Borrowers <- NDG %>% select(Name, Tax.ID, Address.ID, NDG)
Borrowers <- Borrowers %>%
  mutate(Clean_Name = gsub(" ", "_", Name),  # Remove spaces from the Name column
         NDG.PK = paste0(NDG, "_", Clean_Name)) %>%  # Create NDG_PK column
  select(-Clean_Name) %>% 
  select(NDG.PK, NDG, Tax.ID, Name, Address.ID )  # Reorder columns with NDG_PK as the first column

#Creates the Group_Of_Borrowers table
Group_Of_Borrowers <- NDG %>% select(NDG, Category, Group) %>% distinct(NDG, Category, Group)
Group_Of_Borrowers <- Group_Of_Borrowers %>% mutate(NDG = as.numeric(NDG)) 