#Creation of the Entities table according to the metadata we should work with. Loans.R must be ran fist

borrowers <- NDG %>% select (borrower.name, tax.id, city) %>% distinct()
borrowers <- borrowers %>% rename("name" = borrower.name, "cf.piva" = tax.id)
guarantors <- LOANS %>% select (guarantors.name, tax.code.for.guarantors) %>% distinct() %>%
  filter(!is.na(guarantors.name)) 
guarantors <- guarantors %>% rename("name" = guarantors.name, "cf.piva" = tax.code.for.guarantors)
guarantors <- divide_column_by_character(guarantors, c(name, cf.piva), ",")

ENTITIES <- bind_rows(borrowers, guarantors) %>% distinct() #Creates the dataframe with the name column, with unique values
ENTITIES$id.entity <- paste0("c", seq_len(nrow(ENTITIES)))

ENTITIES <- add_type_subject_column(ENTITIES)

ENTITIES <- add_age_column(ENTITIES)

ENTITIES <- add_age_range_column(ENTITIES)

ENTITIES <- add_sex_column(ENTITIES)

ENTITIES <- add_type.pg_column(ENTITIES)

ENTITIES <- ENTITIES %>%
  mutate(
    dummy.info = NA,
    solvency = NA,
    income.pf = NA,
    status.pg = NA,
    date.cessation = NA,
    flag.imputed = NA
  )


# Read the contents of the .paths file
geoMetadataPath_value <- readLines(".paths.txt")
geoMetadataPath_value <- grep("^geoMetadataPath=", geoMetadataPath_value)
geoMetadataPath_value <- sub("^geoMetadataPath=\\s*", "", paths_content[geoMetadataPath_value])

GEO.metadata <- read_excel(geoMetadataPath_value, sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

#To do the mapping I should check first if there are cities with the same name in different provinces/regions
repeated_cities <- c("calliano", "castro", "livo", "peglio", "samone", "san teodoro") 
# repeated_cities <- GEO.metadata %>%
#   group_by(city) %>%
#   summarise(unique_provinces = n_distinct(province)) %>%
#   filter(unique_provinces > 1)

if(any(ENTITIES$city %in% repeated_cities)){
  print("You might have a city assigned to a province incorrectly. Check for: calliano, castro, livo, peglio, samone, san teodoro")
}

# Merge specific columns from 'city_info' into 'ENTITIES'
ENTITIES <- ENTITIES %>%
  left_join(GEO.metadata %>% select(city, province, region, area), by = c("city" = "city"))



ENTITIES$id.entity <- paste0("e", seq_len(nrow(ENTITIES)))

ENTITIES <- ENTITIES %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)