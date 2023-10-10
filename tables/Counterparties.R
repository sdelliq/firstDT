#Creation of the Counterparties table according to the metadata we should work with. Loans.R must be ran fist

# id.bor is of type character, created like b_1 /2/3 etc for a borrower
# name is of type character -> takes names of both borrowers and guarantors
borrowers <- LOANS %>% select (borrower.name) %>% distinct()
borrowers <- borrowers %>% rename("name" = borrower.name)
borrowers$id.bor <- paste0("b", seq_len(nrow(borrowers)))

# id.bor is of type character, created like g_1 /2/3 etc for a guarantor
guarantors <- LOANS %>% select (guarantors.name) %>% distinct() %>% filter (!is.na(guarantors.name)) 
guarantors <- guarantors %>% rename("name" = guarantors.name)
guarantors$id.bor <- paste0("g", seq_len(nrow(guarantors)))

# role is of type factor (Other|Guarantor|Borrower) -> depends on which columns of Loans is (borrower.name or guarantor.name) 
COUNTERPARTIES <- bind_rows(
  mutate(borrowers, role = factor('borrower')),
  mutate(guarantors, role = factor('guarantor'))
)

# n.entities is of type integer -> it's a count of the amount of values usually splited by a comma (can be a -)
COUNTERPARTIES$n.entities <- str_count(COUNTERPARTIES$name, ',') + 1

COUNTERPARTIES <- COUNTERPARTIES %>%
  mutate(
    id.group = NA,
    flag.imputed = NA
  )

# id.counterparty	is of type character, created like c_1 /2/3 etc
COUNTERPARTIES$id.counterparty <- paste0("c", seq_len(nrow(COUNTERPARTIES)))

#I select the columns in the order I want them to be (according to the Metadata)
COUNTERPARTIES <- COUNTERPARTIES %>% select (id.counterparty, id.bor, id.group, role, name, n.entities, flag.imputed)