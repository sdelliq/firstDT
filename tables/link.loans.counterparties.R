#Creation of link.loans.counterparties table


# gather is used to reshape the data by stacking borrower.name and guarantors.name into a single column named name. It removes NAs
link.loans.counterparties <- LOANS %>%
  select(id.loans, borrower.name, guarantors.name) %>%
  tidyr::gather(key = "role", value = "name", -id.loans, na.rm = TRUE) %>%
  left_join(COUNTERPARTIES %>% select(id.counterparty, name), by = "name") %>%
  select(-c(name, role))