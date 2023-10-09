#Creation of the Counterparties table according to the metadata we should work with. Loans.R must be ran fist


# id.counterparty	character -> create like c_1 /2/3 etc
# id.bor	character -> create like b_1 /2/3 etc
# id.group	character -> null
# role	factor (Other|Guarantor|Borrower) -> depends on which columns of Loans is (borrower.name or guarantor.name) 
# name	character -> taken from the columns mentioned before
# n.entities	integer -> it's a count on the split
# flag.imputed	integer -> Flags if the row was imputed (i.e. not present in the original data but generated in order to meet the table's consistency criteria).


borrowers <- LOANS %>% select (borrower.name) %>% distinct()
borrowers <- borrowers %>% rename("name" = borrower.name)
borrowers$id.bor <- paste0("b", seq_len(nrow(borrowers)))

guarantors <- LOANS %>% select (guarantors.name) %>% distinct() %>% filter (!is.na(guarantors.name)) 
guarantors <- guarantors %>% rename("name" = guarantors.name)
guarantors$id.bor <- paste0("g", seq_len(nrow(guarantors)))

COUNTERPARTIES <- bind_rows(mutate(borrowers, role = 'borrower'),
                         mutate(guarantors, role = 'guarantor'))
COUNTERPARTIES$n.entities <- str_count(COUNTERPARTIES$name, ',') + 1

COUNTERPARTIES <- COUNTERPARTIES %>%
  mutate(
    id.group = NA,
    flag.imputed = NA
  )

COUNTERPARTIES$id.counterparty <- paste0("c", seq_len(nrow(COUNTERPARTIES)))

COUNTERPARTIES <- COUNTERPARTIES %>% select (id.counterparty, id.bor, id.group, role, name, n.entities, flag.imputed)
