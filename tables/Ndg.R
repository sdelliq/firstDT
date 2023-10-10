#DEALING WITH THE NDG DATAFRAME
NDG <- NDG.Original

NDG <- as.data.frame(apply(NDG, 2, tolower))
colnames(NDG) <- clean_column_names(colnames(NDG))

#### Separate rows that contain more than 1 value and make them columns
NDG <- NDG %>% 
  separate(borrower.name,c('borrower.name1','borrower.name2','borrower.name3','borrower.name4'),sep = '[-,,]') %>% separate(tax.id,c('TaxID1','TaxID2','TaxID3'),sep = '[-,,]')
### Match columns to pivot and drop NAs
NDG <-  NDG %>% 
  pivot_longer(cols = matches('borrower.n|tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(borrower.name|TaxID)(.*)",values_drop_na =  TRUE)
NDG <- NDG  %>% select(-taxID)

NDG <- rename(NDG, "tax.id" = TaxID)

NDG <- NDG %>% mutate_all(~str_trim(., 'left'))  %>% mutate_all(~str_trim(., 'right'))

NDG <- NDG %>% rename("province" = city, "city" = town, "region" = borrower.s.region)