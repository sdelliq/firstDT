# # Tables:

# -number of borrowers overall
r.numberOfBorrowersOverall <- NDG %>% summarise("number of borrowers" = n_distinct(borrower.name))

# -sum gbv overall
r.sumGBVoverall <- LOANS_FROM_METADATA %>% summarise("GBV sum" = sum(gbv.original))
r.sumGBVoverall <- as.numeric(r.sumGBVoverall)

# -number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
loan.borrower.guarantor <- LOANS %>% 
  select(id.loans, borrower.name, guarantors.name, gbv.original) %>%
  group_by(borrower.name, guarantors.name) %>%
  summarise(
    count_id_loans = n(),
    sum_gbv_original = sum(as.numeric(gbv.original))
  )

r.borrowersByLoans.W.and.WO.guarantors <- loan.borrower.guarantor %>% 
  mutate(has_guarantor = ifelse(!is.na(guarantors.name) & guarantors.name != "", "With Guarantor", "Without Guarantor")) %>%
  group_by(has_guarantor) %>%
  summarise(
    'Amount of Borrowers' = n(),
    'Ratio of Borrowers' = n() / nrow(loan.borrower.guarantor),
    'Amount of Loans' = sum(count_id_loans),
    'Sum of GBV' = sum(sum_gbv_original),
    'Mean of GBV' = mean(sum_gbv_original)
  )


# -number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
uniqueIDbor.type <- LOANS_FROM_METADATA %>% select(id.bor, type) %>% n_distinct()
r.borrowersBy.TypeOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, type, gbv.original) %>%
  group_by(type) %>%
  summarise(
    'Amount of Borrowers' = n_distinct(id.bor),
    'Ratio of Borrowers' = n_distinct(id.bor)/uniqueIDbor.type,
    'Amount of Loans' = n(),
    'Sum of GBV' = sum(as.numeric(gbv.original)),
    'Mean of GBV' = mean(as.numeric(gbv.original))
  )


# -number and ratio of borrowers, number of loans, sum and mean gbv by status of loan
uniqueIDbor.status <- LOANS_FROM_METADATA %>% select(id.bor, status) %>% n_distinct()
r.borrowersBy.StatusOfLoans <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, status, gbv.original) %>%
  group_by(status) %>%
  summarise(
    'Amount of Borrowers' = n_distinct(id.bor),
    'Ratio of Borrowers' = n_distinct(id.bor)/uniqueIDbor.status,
    'Amount of Loans' = n(),
    'Sum of GBV' = sum(as.numeric(gbv.original)),
    'Mean of GBV' = mean(as.numeric(gbv.original))
  )


# -number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
r.borrowersBy.GBVclusters <- LOANS_FROM_METADATA %>% 
  select(id.loans, id.bor, gbv.original) %>%
  mutate(
    Quartile = ntile(gbv.original, 3),
    GBV_Range = case_when(
      Quartile == 1 ~ paste0("0 - ", round(quantile(gbv.original, 0.25))),
      Quartile == 2 ~ paste0(round(quantile(gbv.original, 0.25)) + 1, " - ", round(quantile(gbv.original, 0.5))),
      Quartile == 3 ~ paste0(round(quantile(gbv.original, 0.5)) + 1, " - ", round(max(gbv.original)))
    )
  )

uniqueIDbor.quartile <- r.borrowersBy.GBVclusters %>% select(id.bor,  Quartile) %>% n_distinct()

r.borrowersBy.GBVclusters <-  r.borrowersBy.GBVclusters %>%    
  group_by(Quartile, GBV_Range) %>%
      summarise(
        'Amount of Borrowers' = n_distinct(id.bor),
        'Ratio of Borrowers' = n_distinct(id.bor)/uniqueIDbor.quartile,
        'Amount of Loans' = n(),
        'Sum of GBV' = sum(as.numeric(gbv.original)),
        'Mean of GBV' = mean(as.numeric(gbv.original))
      )


# -number and ratio of borrowers, number of loans, by area
borrowerANDarea <- COUNTERPARTIES %>% select (id.counterparty, role) %>% filter(role == "borrower") %>%
  left_join(link.counterparties.entities, by= "id.counterparty") %>% 
    left_join(ENTITIES %>% select(id.entity, area), by= "id.entity") %>% 
      left_join(link.loans.counterparties, by="id.counterparty")


uniqueIDbor.area <- borrowerANDarea %>% select(id.counterparty, area) %>% n_distinct()
r.borrowersBy.Area <- borrowerANDarea %>% 
  group_by(area) %>%
  summarise(
    'Amount of Borrowers' = n_distinct(id.counterparty),
    'Ratio of Borrowers' = n_distinct(id.counterparty)/uniqueIDbor.area,
    'Amount of Loans' = n_distinct(id.loans),
  )


# -a pivot(cross table or contingency table (https://en.wikipedia.org/wiki/Contingency_table)) of sum gvb by gbv clusters that you create and loans with/without guarantors







excel_file_path <- "report/reportTables.xlsx"
wb <- createWorkbook()
writeWB(wb, "Tables Report", )
saveWorkbook(wb, file = excel_file_path)