
#Writing the feather files of the dataframes
write_feather(ENTITIES, "feather/entities.feather")
write_feather(COUNTERPARTIES, "feather/counterparties.feather")
write_feather(LOANS, "feather/loans.feather")
write_feather(link.counterparties.entities, "feather/link_c_e.feather")
write_feather(link.loans.counterparties, "feather/link_l_c.feather")

#Reading the feather files
f_entities <- read_feather("feather/entities.feather")
f_counterparties <- read_feather("feather/counterparties.feather")
f_loans <- read_feather("feather/loans.feather")
f_link_c_e <- read_feather("feather/link_c_e.feather")
f_link_l_c <- read_feather("feather/link_l_c.feather")

#Running checks 
# - Counterparties Table

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
            cat("Counterparty", cp1, "is a subset of Counterparty", cp2, "\n")
          }
        }
      }
    }
  }
}

# Example 
df <- data.frame(
  id.counterparty = c(1, 1, 2, 2, 2, 3, 4, 5, 5),
  id.entity = c("A", "B", "A", "B", "C", "E", "K", "B", "C")
)

check_subset_relationship(df)


#The ID must exist in Link_Counterparty_Entity
check_counterparties <- function(df, counterparties_df) {
  unique_counterparties <- unique(df$id.counterparty)
  missing_counterparties <- setdiff(unique_counterparties, counterparties_df$id.counterparty)
  if (length(missing_counterparties) == 0) {
    print("All counterparty IDs are present in the counterparties_df.")
  } else {
    cat("Missing counterparty IDs:", missing_counterparties, "\n")
  }
}
check_counterparties(f_link_c_e, f_counterparties)


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
      cat("Mismatch for Counterparty", counterparty_id, ": Expected", expected_count, "but found", count_in_link_c_e, "\n")
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
# Example usage
df <- data.frame(
  name = c("Some Company", "Another COMPANY", "S.R.L Corporation", "DI Solutions", "sncm Logistics")
)

check_name_format(df)



# - Entities Table

#The ID must exist in Link_Counterparty_Entity
check_counterparties(f_link_c_e, f_entities)