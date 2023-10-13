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
r.sumGBVoverall <- as.data.frame(r.sumGBVoverall)


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
  )
summary_row <- r.borrowersByLoans.W.and.WO.guarantors %>%
  summarize(
    has_guarantor = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersByLoans.W.and.WO.guarantors <- rbind(summary_row, r.borrowersByLoans.W.and.WO.guarantors) %>% rename (" " = has_guarantor)


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
  ) 
summary_row <- r.borrowersBy.TypeOfLoans %>%
  summarize(
    type = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersBy.TypeOfLoans <- rbind(summary_row, r.borrowersBy.TypeOfLoans) %>% rename ("Loan Type" = type)


# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
quantiles <- quantile(borrowerMartTable$gbv.original, c(1/3, 2/3))
print(quantiles)
breaks <- c(0, 60000, 360000, Inf)
range.gbv <- c("0 - 60k", "60k - 360k", "360k - Inf")
borrowerWithGBVbucket <- borrowerMartTable %>%
  select(id.loans, id.counterparty, gbv.original) 
borrowerWithGBVbucket$gbv_range <- cut(borrowerWithGBVbucket$gbv.original, breaks = breaks, labels = range.gbv)

uniqueIDbor.combination <- borrowerWithGBVbucket %>% select(id.counterparty,gbv_range) %>% n_distinct()
r.borrowersBy.GBVclusters <-  borrowerWithGBVbucket %>%    
  group_by(gbv_range) %>%
      summarise(
        '# Borrowers' = n_distinct(id.counterparty),
        '% Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.combination,
        '# Loans' = sum(id.loans),
        'GBV Sum' = sum(as.numeric(gbv.original)),
        'GBV Mean' = mean(as.numeric(gbv.original))
      )
summary_row <- r.borrowersBy.GBVclusters %>%
  summarize(
    gbv_range = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersBy.GBVclusters <- rbind(summary_row, r.borrowersBy.GBVclusters) %>% rename ("GBV" = gbv_range)


# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan and gbv clusters
r.borrowersBy.StatusOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, status, gbv.original) 
uniqueIDbor.combination <- r.borrowersBy.StatusOfLoans %>% select(id.bor, status) %>% n_distinct()
r.borrowersBy.StatusOfLoans$gbv_range <- cut(r.borrowersBy.StatusOfLoans$gbv.original, breaks = breaks, labels = range.gbv)
r.borrowersBy.StatusOfLoans <- r.borrowersBy.StatusOfLoans %>%
  group_by(status, gbv_range) %>%
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '% Borrowers' = n_distinct(id.bor)/uniqueIDbor.combination,
    '# Loans' = n(),
    'GBV Sum' = sum(as.numeric(gbv.original)),
    'GBV Mean' = mean(as.numeric(gbv.original))
  )  
summary_row <- r.borrowersBy.StatusOfLoans %>%
  group_by(status) %>%
  summarize(
    gbv_range = n(),
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
summary_row$status <- ifelse(summary_row$status == "utp", "Subtotal UTP", "Subtotal Bad")
r.borrowersBy.StatusOfLoans <- rbind(summary_row, r.borrowersBy.StatusOfLoans) 
summary_row <- summary_row %>% summarize(
    status= "Totals",
    across(c(gbv_range, `# Borrowers`, `% Borrowers`, `# Loans`, `GBV Sum`), sum),
    `GBV Mean` = mean(`GBV Mean`)
  ) 
r.borrowersBy.StatusOfLoans <- rbind(summary_row, r.borrowersBy.StatusOfLoans) %>% rename ("Loan Status" = status)
r.borrowersBy.StatusOfLoans <- r.borrowersBy.StatusOfLoans %>%
  mutate(Sort_Order = case_when(
    `Loan Status` == "Totals" ~ 1,
    `Loan Status` == "bad" ~ 2,
    `Loan Status` == "Subtotal Bad" ~ 3,
    `Loan Status` == "utp" ~ 4,
    `Loan Status` == "Subtotal UTP" ~ 5
  )) %>%
  arrange(Sort_Order) %>%
  select(-Sort_Order)

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
summary_row <- r.borrowersBy.Area %>%
  summarize(
    area = "Totals",
    across(c(`# Borrowers`, `% Borrowers`, `# Loans`), sum)
  ) 
r.borrowersBy.Area <- rbind(summary_row, r.borrowersBy.Area) 

# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors
pivot_table <- borrowerMartTable %>%
  mutate(bucket = cut(gbv.original, breaks = breaks, labels = range.gbv, include.lowest = TRUE))
pivot_table <- pivot_table %>%
  group_by(flag_Guarantor, bucket) %>%
  summarise(gbv_sum = sum(gbv.original)) %>%
  pivot_wider(
    names_from = bucket,
    values_from = gbv_sum,
    values_fill = 0
  ) 
pivot_table$flag_Guarantor <- ifelse(pivot_table$flag_Guarantor == TRUE, "With Guarantor", "Without Guarantor")

summary_row <- pivot_table %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) %>%
  summarize(
    flag_Guarantor = "Totals",
    across(range.gbv, sum)
  ) 
pivot_table <- rbind(summary_row, pivot_table) 

