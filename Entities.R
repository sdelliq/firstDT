#Creation of the Entities table according to the metadata we should work with. Loans.R must be ran fist

borrowers <- NDG %>% select (borrower.name, tax.id, city) %>% distinct()
borrowers <- borrowers %>% rename("name" = borrower.name, "cf.piva" = tax.id)
guarantors <- LOANS %>% select (guarantors.name, tax.code.for.guarantors) %>% distinct() %>%
  filter(!is.na(guarantors.name)) 
guarantors <- guarantors %>% rename("name" = guarantors.name, "cf.piva" = tax.code.for.guarantors)
guarantors <- divide_column_by_character(guarantors, c(name, cf.piva), ",")

ENTITIES <- bind_rows(borrowers, guarantors) %>% distinct() #Creates the dataframe with the name column, with unique values
ENTITIES$id.entity <- paste0("c", seq_len(nrow(ENTITIES)))
ENTITIES$type.subject <- sapply(ENTITIES$cf.piva, function(x){
  if (is.na(x)) {
    type.subject <- NA
  } else if (any(str_detect(x, "confidi|fidi"))) {
    type.subject <- "confidi"
  } else if (str_length(x) == 10) {
    type.subject <- "corporate"
  } else {
    type.subject <- "individual"
  }
})


ENTITIES$age <- sapply(ENTITIES$cf.piva, function(x){
  if(!is.na(ENTITIES$type.subject[x]) && ENTITIES$type.subject[x] == "individual"){
    yearOfBirth <- as.numeric(str_sub(x, start= 7L, end = 8L))
    currentYear <- as.numeric(format(as.Date(Sys.time()), "%Y"))
    if (yearOfBirth >= 0 && yearOfBirth <= (currentYear-2018)) {
      yearOfBirth <- 2000 + yearOfBirth
    } else {
      yearOfBirth <- 1900 + yearOfBirth
    }
    ENTITIES$age <- currentYear - yearOfBirth
  }
  else{
    ENTITIES$age <- NA
  }
})


# Define the age categories
breaks <- c(0, 25, 50, 65, 75, Inf)
labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
ENTITIES$range.age <- cut(ENTITIES$age, breaks = breaks, labels = labels, right = FALSE)


ENTITIES <- ENTITIES %>% 
  mutate(sex = case_when(
    !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) > 40 ~ "f",
    !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) <= 40 ~ "m",
    TRUE ~ NA_character_
  ))

ENTITIES <- ENTITIES %>% 
  mutate(type.pg = case_when(
    str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
    str_detect(name, "d.i|d.i.")  ~ "di",
    str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
    str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
    str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
    str_detect(name, " sc |s.c|s.c.|scs")  ~ "sc",
    TRUE ~ NA_character_
  ))

ENTITIES <- ENTITIES %>%
  mutate(
    dummy.info = NA,
    solvency = NA,
    income.pf = NA,
    status.pg = NA,
    date.cessation = NA,
    flag.imputed = NA
  )


#To do the mapping I should check first if there are cities with the same name in different provinces/regions

GEO.metadata <- read_excel("C:/Users/sophia.dellarciprete/iQera Italia SpA/Advisory Team - formation_2023/data/data_model/Metadata_2022_12_21.xlsx", sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

# Merge specific columns from 'city_info' into 'ENTITIES'
ENTITIES <- ENTITIES %>%
  left_join(GEO.metadata %>% select(city, province, region, area), by = c("city" = "city"))

ENTITIES$id.entity <- paste0("e", seq_len(nrow(ENTITIES)))

ENTITIES <- ENTITIES %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)