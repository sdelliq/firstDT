#Creation of the Entities table according to the metadata we should work with. Loans.R must be ran fist

# name= type:character. It's the individual’s name (borrower or guarantor)
# cf.piva= type:character. Takes the codice fiscale or partita iva from the borrower/guarantor
borrowers <- NDG %>% select (borrower.name, tax.id, city) %>% distinct()
borrowers <- borrowers %>% rename("name" = borrower.name, "cf.piva" = tax.id)
guarantors <- LOANS %>% select (guarantors.name, tax.code.for.guarantors) %>% distinct() %>%
  filter(!is.na(guarantors.name)) 
guarantors <- guarantors %>% rename("name" = guarantors.name, "cf.piva" = tax.code.for.guarantors)
guarantors <- divide_column_by_character(guarantors, c(name, cf.piva), ",")

#Creates the dataframe with the name and cf.piva column, with unique values
ENTITIES <- bind_rows(borrowers, guarantors) %>% distinct() 

# id_entity: character created as e1/e2/etc
ENTITIES$id.entity <- paste0("e", seq_len(nrow(ENTITIES)))

# type.subject: if there’s a CF is a person, if its piva it’s corporate
ENTITIES <- add_type_subject_column(ENTITIES)
ENTITIES$type.subject <- factor(ENTITIES$type.subject, levels = c('other', 'confidi', 'corporate', 'individual'))

#age: number taken from the codice fiscale
ENTITIES <- add_age_column(ENTITIES)

# range-age= type:factor. Taken also from the CF
ENTITIES <- add_age_range_column(ENTITIES)

# sex= type: factor. Taken also from the CF
ENTITIES <- add_sex_column(ENTITIES)
ENTITIES$sex <- factor(ENTITIES$sex, levels = c('m', 'f'))

# type:pg (persona giuridica) = type:factor. take SRL, S.R.L., SRLS and more. 
ENTITIES <- add_type.pg_column(ENTITIES)
ENTITIES$type.pg <- factor(ENTITIES$type.pg, levels = c('other', 'di', 'ss', 'sas', 'snc', 'spa', 'srl', 'sc'))

ENTITIES <- ENTITIES %>%
  mutate(
    dummy.info = NA,
    solvency = NA,
    income.pf = NA,
    status.pg = NA,
    date.cessation = NA,
    flag.imputed = NA
  )


# city, province, region, area: take it from NDG (Town is City, City is Province). Try to use the mapping with the table from the metadata geo sheet

# Read the contents of the .paths file to get the location of the metadata file, to then read the Geo sheet
paths_content <- readLines(".paths.txt")
geoMetadataPath_line <- grep("^geoMetadataPath=", paths_content)
geoMetadataPath_value <- sub("^geoMetadataPath=\\s*", "", paths_content[geoMetadataPath_line])

GEO.metadata <- read_excel(geoMetadataPath_value, sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

#I first check if there are cities with the same name in different provinces/regions - I saved the result in a list, since I don't think it's worth re.running.
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

ENTITIES$province <- factor(ENTITIES$province, levels = c(
  "abroad", "agrigento", "alessandria", "ancona", "aosta", "arezzo", "ascoli piceno", "asti", "avellino",
  "bari", "barletta andria trani", "belluno", "benevento", "bergamo", "biella", "bologna", "bolzano", "brescia",
  "brindisi", "cagliari", "caltanissetta", "campobasso", "caserta", "catania", "catanzaro", "chieti", "como", 
  "cosenza", "cremona", "crotone", "cuneo", "enna", "fermo", "ferrara", "firenze", "foggia", "forli' cesena", 
  "frosinone", "genova", "gorizia", "grosseto", "imperia", "isernia", "l'aquila", "la spezia", "latina", "lecce", 
  "lecco", "livorno", "lodi", "lucca", "macerata", "mantova", "massa carrara", "matera", "messina", "milano", 
  "modena", "monza brianza", "napoli", "novara", "nuoro", "oristano", "padova", "palermo", "parma", "pavia", 
  "perugia", "pesaro urbino", "pescara", "piacenza", "pisa", "pistoia", "pordenone", "potenza", "prato", "ragusa", 
  "ravenna", "reggio calabria", "reggio emilia", "rieti", "rimini", "roma", "rovigo", "salerno", "sassari", "savona", 
  "siena", "siracusa", "sondrio", "sud sardegna", "taranto", "teramo", "terni", "torino", "trapani", "trento", 
  "treviso", "trieste", "udine", "varese", "venezia", "verbano cusio ossola", "vercelli", "verona", "vibo valentia", 
  "vicenza", "viterbo"
))

ENTITIES$region <- factor(ENTITIES$region, levels =c(
  "abroad", "abruzzo", "basilicata", "calabria", "campania", "emilia romagna", "friuli venezia giulia", 
  "lazio", "liguria", "lombardia", "marche", "molise", "piemonte", "puglia", "sardegna", "sicilia", "toscana", 
  "trentino alto adige", "umbria", "valle d'aosta", "veneto"
))

ENTITIES$area <- factor(ENTITIES$area, levels =c(
  "abroad", "islands", "south", "center", "north-east", "north-west"
))



#I select the columns in the order I want (the order in Metadata)
ENTITIES <- ENTITIES %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)