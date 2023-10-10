
link.counterparties.entities <- COUNTERPARTIES %>% select (id.counterparty, name)
link.counterparties.entities <- divide_column_by_character(link.counterparties.entities, name, ",")
link.counterparties.entities <- divide_column_by_character(link.counterparties.entities, name, "-")
link.counterparties.entities <- link.counterparties.entities %>% 
  left_join(ENTITIES %>% select(id.entity, name), by= c("name" = "name"))

link.counterparties.entities <- link.counterparties.entities %>% select(-name)
