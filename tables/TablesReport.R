# # Tables:
#Creation of a Mart Table with Borrowers information
borrowerMartTable <- COUNTERPARTIES %>% select (id.counterparty, role) %>%
  left_join(link.loans.counterparties, by= "id.counterparty") %>% 
  group_by(id.loans) %>%
  mutate(flag_Guarantor = ifelse(n() > 1, TRUE, FALSE)) %>%
  filter(role == "borrower") %>% 
  left_join(LOANS_FROM_METADATA %>% select (id.loans, gbv.original), by= "id.loans")

borrowerMartTable <- borrowerMartTable %>%  group_by(id.counterparty) %>% summarise(
  'id.loans' = n(),
  'gbv.original' = sum(gbv.original),
  flag_Guarantor = ifelse(any(flag_Guarantor), TRUE, FALSE) )
print(sum(borrowerMartTable$gbv.original))


# -number of borrowers overall
r.numberOfBorrowersOverall <- NDG %>% summarise("# Borrowers" = n_distinct(ndg))


# -sum gbv overall
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV Sum" = sum(gbv.original))
r.sumGBVoverall <- as.numeric(r.sumGBVoverall)


# -number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
r.borrowersByLoans.W.and.WO.guarantors <- borrowerMartTable %>% 
  mutate(
    has_guarantor = if_else(flag_Guarantor, "With Guarantor", "Without Guarantor")
  ) %>%
  group_by(has_guarantor) %>%
  summarise(
    '# Borrowers' = n(),
    '% Borrowers' = n() / nrow(borrowerMartTable),
    '# Loans' = sum(id.loans),
    'GBV Sum' = sum(gbv.original),
    'GBV Mean' = mean(gbv.original)
  ) %>%
  bind_rows(
    summarise(
      .,
      has_guarantor = "Totals",
      across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
      `GBV Mean` = mean(`GBV Mean`)
    )
  ) %>% 
  rename(" " = has_guarantor)


# -number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
uniqueIDbor.combination <- LOANS_FROM_METADATA %>% select(id.bor, type) %>% n_distinct()

r.borrowersBy.TypeOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, type, gbv.original) %>%
  group_by(type) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor) / uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  ) %>%
  bind_rows(
    summarise(
      .,
      type = "Totals",
      across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
      `GBV Mean` = mean(`GBV Mean`)
    )
  ) %>% 
  rename("Loan Type" = type)


# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan
uniqueIDbor.combination <- LOANS_FROM_METADATA %>% select(id.bor, status) %>% n_distinct()
r.borrowersBy.StatusOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, status, gbv.original) %>%
  group_by(status) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor)/uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )  %>%
  bind_rows(
    summarise(
      .,
      type = "Totals",
      across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
      `GBV Mean` = mean(`GBV Mean`)
    )
  ) %>% 
  rename ("Loan Status" = status)


# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
r.borrowersBy.GBVclusters <- borrowerMartTable %>%
  select(id.loans, id.counterparty, gbv.original) %>%
  mutate(Quartile = ntile(gbv.original, 3)) %>%
  mutate(
    GBV_Range = cut(
      gbv.original,
      breaks = quantile(gbv.original, probs = c(0, 0.25, 0.5, 1)),
      labels = c(
        paste0("0 - ", round(quantile(gbv.original, 0.25))),
        paste0(round(quantile(gbv.original, 0.25) + 1), " - ", round(quantile(gbv.original, 0.5))),
        paste0(round(quantile(gbv.original, 0.5) + 1), " - ", round(max(gbv.original)))
      ),
      include.lowest = TRUE
    )
  )

uniqueIDbor.combination <- r.borrowersBy.GBVclusters %>% select(id.counterparty,  Quartile) %>% n_distinct()

r.borrowersBy.GBVclusters <-  r.borrowersBy.GBVclusters %>%    
  group_by(GBV_Range) %>%
      summarise(
        '# Borrowers' = n_distinct(id.counterparty),
        '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
        '# Loans' = n(),
        'GBV Sum' = sum(as.numeric(gbv.original)),
        'GBV Mean' = mean(as.numeric(gbv.original))
      )


# -number and ratio of borrowers, number of loans, by area
borrowerANDarea <- COUNTERPARTIES %>% select (id.counterparty, role) %>% filter(role == "borrower") %>%
  left_join(link.counterparties.entities, by= "id.counterparty") %>% 
    left_join(ENTITIES %>% select(id.entity, area), by= "id.entity") %>% 
      left_join(link.loans.counterparties, by="id.counterparty")


uniqueIDbor.combination <- borrowerANDarea %>% select(id.counterparty, area) %>% n_distinct()
r.borrowersBy.Area <- borrowerANDarea %>% 
  group_by(area) %>%
  summarise(
    '# Borrowers' = n_distinct(id.counterparty),
    '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
    '# Loans' = n_distinct(id.loans),
  )


# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors
quartiles <- r.borrowersBy.GBVclusters$GBV_Range
loan.borrower.guarantor <- borrowerMartTable 
quartiles_levels <- levels(quartiles)
loan.borrower.guarantor[, quartiles_levels] <- 0

borrowerMartTable <- borrowerMartTable %>%
  mutate(bucket = cut(gbv.original, breaks = c(0, 51539, 121208, Inf), labels = c("0-51539", "51540-121208", "121209-2687170"), include.lowest = TRUE))

pivot_table <- borrowerMartTable %>%
  group_by(flag_Guarantor, bucket) %>%
  summarise(gbv_sum = sum(gbv.original)) %>%
  pivot_wider(
    names_from = bucket,
    values_from = gbv_sum,
    values_fill = 0
  ) 

pivot_table$flag_Guarantor <- ifelse(pivot_table$flag_Guarantor == TRUE, "With Guarantor", "Without Guarantor")
pivot_table <- pivot_table %>% rename (" " = flag_Guarantor)

excel_file_path <- "report/reportTables.xlsx"
wb <- createWorkbook()
writeWB(wb, "Tables Report", )
saveWorkbook(wb, file = excel_file_path)