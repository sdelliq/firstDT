###-----------------------------------------------------------------------###
#-----      Writing the feather files of the dataframes                -----         
###-----------------------------------------------------------------------###

# write_feather(ENTITIES, "feather/entities.feather")
# write_feather(COUNTERPARTIES, "feather/counterparties.feather")
# write_feather(LOANS, "feather/loans.feather")
# write_feather(link.counterparties.entities, "feather/link_c_e.feather")
# write_feather(link.loans.counterparties, "feather/link_l_c.feather")

###-----------------------------------------------------------------------###
#-----              Reading the feather files                          -----         
###-----------------------------------------------------------------------###

f_entities <- read_feather("feather/entities.feather")
f_counterparties <- read_feather("feather/counterparties.feather")
f_loans <- read_feather("feather/loans.feather")
f_link_c_e <- read_feather("feather/link_c_e.feather")
f_link_l_c <- read_feather("feather/link_l_c.feather")

###-----------------------------------------------------------------------###
#-----                     Running checks                              -----         
###-----------------------------------------------------------------------###

###-----------------------------------------------------------------------###
#-----      Common Functions (usefull for more than one table)          -----         
###-----------------------------------------------------------------------###


###-----------------------------------------------------------------------###
#-----                  Counterparties Table                           -----         
###-----------------------------------------------------------------------###

# -- Within a given ID_Bor, ID_Counterparties should not be redundant from the point of view of entities (i.e. if all entities in ID1 coincide with part of the entities in ID2, ID2 should be dropped, and its guarantee links attributed to ID1)

check_subset_relationship <- function(df) {
  id_counterparty <- df$id.counterparty
  id_entity <- df$id.entity
  
  if (n_distinct(id_entity) == length(id_counterparty)) {
    print("There are no redundant ID_Bor, ID_Counterparties")
  } else {
    # Create a list of entities for each counterparty
    counterparty_entities <- split(id_entity, id_counterparty)
    
    # Check if one counterparty is a subset of another
    for (cp1 in names(counterparty_entities)) {
      for (cp2 in names(counterparty_entities)) {
        if (cp1 != cp2) {
          subset_check <- all(counterparty_entities[[cp1]] %in% counterparty_entities[[cp2]])
          if (subset_check) {
            cat("Redundancy found. Counterparty", cp1, "is a subset of Counterparty", cp2, "\n")
          }
        }
      }
    }
  }
}
# # Example df to see the code work
# df <- data.frame(
#   id.counterparty = c(1, 1, 2, 2, 2, 3, 4, 5, 5),
#   id.entity = c("A", "B", "A", "B", "C", "E", "K", "B", "C")
# )
check_subset_relationship(f_counterparties)


#The ID must exist in Link_Counterparty_Entity
check_id_inLinkingTable <- function(linkingTable, entity_or_counterparty_df, id_column) {
  unique_ids <- unique(linkingTable[[id_column]])
  missing_ids <- setdiff(unique_ids, entity_or_counterparty_df[[id_column]])
  
  if (length(missing_ids) == 0) {
    return(paste0("Every ", id_column, " from ", deparse(substitute(entity_or_counterparty_df)), " is present in the linking table"))
  } else {
    return(paste0("Not every ", id_column, "from", deparse(substitute(entity_or_counterparty_df)), " is present in the linking table. Missing ", id_column, "s:", missing_ids, "\n"))
  }
}

# Example usage with modified names
check_id_inLinkingTable(f_link_c_e, f_counterparties, "id.counterparty")


#it measures, for a given ID_Counterparty, the number of ID_Entity in Link_Counterparty_Entity
check_entity_counts <- function(counterparties, link_c_e) {
  # Extract unique counterparty IDs and their corresponding n.entities values
  unique_counterparties <- unique(counterparties$id.counterparty)
  entity_counts <- counterparties$n.entities[match(unique_counterparties, counterparties$id.counterparty)]
  
  # Check if the count of appearances in another_df equals n.entities
  for (counterparty_id in unique_counterparties) {
    count_in_link_c_e <- sum(link_c_e$id.counterparty == counterparty_id)
    expected_count <- entity_counts[counterparty_id == unique_counterparties]
    
    if (count_in_link_c_e != expected_count) {
      cat("Mismatch for Counterparty in Link_Counterparty_Entity", counterparty_id, ": Expected", expected_count, "but found", count_in_link_c_e, "\n")
    }
  }
}
check_entity_counts(f_counterparties, f_link_c_e)


#Uppercase letters allowed ("Mario Rossi" or "Random SRL" allowed)
check_name_format <- function(df) {
  not_lowercase <- df$name != tolower(df$name)
  if (any(not_lowercase)) {
    df$name_no_dots <- gsub("\\.", "", df$name)
    # Check if words in the "name" column are capitalized
    capitalized_words <- sapply(strsplit(df$name_no_dots[not_lowercase], "\\s+"), function(x) all(grepl("^[[:upper:]]", x)))
    # Check if part of the name is in all caps and matches a type of company
    caps_and_patterns <- grepl("\\b(SRL|SNCM|DI)\\b", df$name_no_dots[not_lowercase])
    
    cat("Rows with 'name' not in lowercase:", which(not_lowercase), "\n")
    cat("Rows with capitalized words:", which(capitalized_words), "\n")
    cat("Rows with 'SRL', 'SNCM', 'DI' patterns:", which(caps_and_patterns), "\n")
  } else {
    print("All 'name' values are in lowercase.")
  }
}
# # Example to see the code work
# df <- data.frame(
#   name = c("Some Company", "Another COMPANY", "S.R.L Corporation", "DI Solutions", "sncm Logistics")
# )
check_name_format(f_counterparties)


###-----------------------------------------------------------------------###
#-----                     Entities Table                              -----         
###-----------------------------------------------------------------------###


#The ID must exist in Link_Counterparty_Entity
check_counterparties(f_link_c_e, f_entities)


#For the cf.piva column, Unless NULL, there should not be duplicates
check_duplicates_cf.piva <- function(data, column) {
  notNull <- data[[column]][!is.na(data[[column]])]
  
  duplicates <- notNull[duplicated(notNull)]
  
  if (length(duplicates) == 0) {
    return(paste("There are no duplicates in the", column, "column."))
  } else {
    return(paste("Duplicate values found in the", column, "column:", toString(unique(duplicates))))
  }
}
#Dataframe to see the code work
# df <- data.frame(
#   cf.piva = c(123456789, 987654321, 123456789, 555555555, NA, 987654321),
#   other_column = c("A", "B", "C", "D", "E", "F")
# )
check_duplicates_cf.piva(f_entities, "cf.piva")


#There must be at least one entity for every id_counterparty.
check_associations <- function(ids_in_counterparty, ids_in_linking_table, id_type) {
  if (n_distinct(ids_in_counterparty) == n_distinct(ids_in_linking_table)) {
    return(paste("There is at least one", id_type, "for every", id_type))
  } else {
    missing_ids <- setdiff(ids_in_counterparty, ids_in_linking_table)
    return(paste("Some", id_type, "are not associated with at least one entity. Here's the list:", missing_ids))
  }
}
check_associations(f_counterparties$id.counterparty, f_link_c_e$id.counterparty, "id_counterparty")


#'- if cf.piva is a fiscal code ==> individual;
# - if cf.piva is a p.iva ==> corporate, confidi, or other (NOT individual)
# - if cf.piva is N/A, no rules apply (there can't be inconsistency in this case)
cf.piva_and_type <- f_entities %>% select(cf.piva, type.subject) %>% filter(nchar(cf.piva) == 16) 
non_individuals <- cf.piva_and_type %>% filter(type.subject != "individual")
if (nrow(non_individuals) == 0) {
  print("All CFs belong to individuals")
} else {
  print(paste("The following CFs are not classified as individuals:", toString(non_individuals$cf.piva)))
}




