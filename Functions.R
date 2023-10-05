#Functions

#Reads a file a returns a List with one dataframe for each excel sheet
read_doc_and_save_df <- function(file_path) {
  sheets <- excel_sheets(file_path)
  tibble_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
  # Convert each sheet to a data frame
  DT <- lapply(tibble_list, as.data.frame)
  # Set names for the list elements based on sheet names
  names(DT) <- sheets
  return(DT)
}
#Running example:
#file <- 'Data/file.xlsx'
#DT <- read_doc_and_save_df(file)


#Deletes a given word from all the columns in which it appears
delete_word_from_col <- function(names, wordToDel) {
  names <- gsub(wordToDel,"", names)
  return(names)
}
#Running example:
#colnames(NDG_N) <- delete_word_from_col(colnames(NDG_N),"NDG.")


clean_column_names <- function(names) {
  # Use a regular expression to find positions between consecutive uppercase and lowercase letters to solve for example BorrowerName
  pattern <- "(?<=[a-z])(?=[A-Z])"
  names <- gsub(pattern, ".", names, perl = TRUE)
  
  # Make.names handles other invalid characters and ensures uniqueness
  names <- make.names(names, unique = TRUE)
  # Use gsub to replace consecutive dots with a single dot and remove trailing dots
  names <- gsub("\\.{2,}", ".", names)
  names <- gsub("\\.$", "", names)
  
  # Convert all characters to lowercase
  names <- tolower(names)
  
  return(names)
}
# Running example:
# colnames(df) <- clean_column_names(colnames(df))


# Identifies columns whose percentage in terms of NA values exceeds a certain threshold imputed as parameter
columns_to_delete <- function(dataframe, threshold = 0.8) {
  total_rows <- nrow(dataframe)
  na_threshold <- total_rows * threshold
  na_columns <- colnames(dataframe)[colSums(is.na(dataframe)) >= na_threshold]
  return(na_columns)
}
# Running example:
#cols_to_delete <-  columns_to_delete(dataframe,threshold = 0.8)
#print(cols_to_delete)

#Returns the possible primary keys of a given dataframe, in the form of a list with the column, its uniqueness and weather it has NAs or not
possiblePKs <- function(df){ 
  # Calculate the ratio of uniqueness as a numeric vector
  uniqueness_ratios <- apply(df, 2, function(x) {
    if (any(is.na(x))) {
      ((length(unique(x)) - sum(is.na(x))) / length(x)) * 100
    } else {
      length(unique(x)) / length(x) * 100
    }
  })
  # Combine column names and ratios into a data frame
  PK <- data.frame(Column = names(uniqueness_ratios), Ratio = sprintf("%.2f%%", uniqueness_ratios))
  # Add a new column indicating if there are NAs in the corresponding columns
  PK$Has_NA <- apply(df, 2, function(x) any(is.na(x)))
  #Only gets ratios over 80
  PK <- PK %>% filter(as.numeric(gsub("%", "", Ratio))>80)
  # Get the indices that sort the ratios in descending order
  sorted_indices <- order(as.numeric(gsub("%", "", PK$Ratio)), decreasing = TRUE)
  # Reorder PK based on the sorted indices
  PK <- PK[sorted_indices, ]
  return(PK)
}


# Function to split values, and create two rows when there's one with multiple values divided by a separator
divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE)
}
# Running example:
# NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures.CODE", "\\+ ")


#Compares one given column with the rest of the columns on the given dataframe and returns a one row df with the functional dependencies
functionalDependencies_Of_Column <- function(df, column_to_asses) {
    result <- df %>%
      group_by_at(vars(column_to_asses)) %>%
      summarize(across(everything(), ~n_distinct(.x), .names = "{.col}"), .groups = "drop") 
    result <- result %>% summarize(across(-1, ~ nrow(result)/sum(.))) %>% 
      mutate(!!column_to_asses := 1)
    return (result)
}
# Running example:
# result <- functionalDependencies_Of_Column(df, column_to_asses)


#Creates a matrix with the functional dependencies of the whole dataframe
functionalDependencies_Matrix <- function(df) {
  column_names <- names(df)
  # Create an empty matrix with rows and columns named after the columns of the dataframe
  fd_matrix <- matrix(0, nrow = length(column_names), ncol = length(column_names), dimnames = list(column_names, column_names))
  for (column in colnames(df)){
    results <- functionalDependencies_Of_Column(df, column)
    for (row in colnames(results) ){
      fd_matrix[row, column] <- as.numeric(results[row])
    }
  }
  #Creates a row with the sum of what we're certain are functional dependencies
  certain_functional_dependencies <- apply(fd_matrix, 2, function(x) {
    sum(x == 1)
  })
  fd_matrix <- rbind(certain_functional_dependencies, fd_matrix) %>% round(digits = 2)
}
# Running example:
#matrix_fd <- functionalDependencies_Matrix(LOANS)


#Returns a list of dataframes with percentages of functional dependencies higher than the percentage indicated 
namesOfTablesToBe <- function(df, percentage){
  matrix_fd <- functionalDependencies_Matrix(df)
  certain_functional_dependencies <- matrix_fd[1, ]
  # Find column names where the value is higher than the percentage
  selected_columns <- names(certain_functional_dependencies[certain_functional_dependencies/nrow(matrix_fd) > percentage])
  
  tables_to_be <- list()
  for (col_name in selected_columns) {
    rows_with_ones <- rownames(matrix_fd)[matrix_fd[, col_name] == 1]
    # Extract values corresponding to the selected rows
    values <- matrix_fd[rows_with_ones, col_name]
    result_df <- data.frame(Value = values)
    # Check it there's already a dataframe like that, and only stores it if there isn't
    same <- FALSE
    for (table2 in tables_to_be){
      if(identical(result_df, table2)){
        same <- TRUE
        break
      }
    }
    if(same == FALSE){
      tables_to_be[[col_name]] <- result_df
    }
  }
  return (tables_to_be)
}
# Running example:
# tablesToBe  <- namesOfTablesToBe(LOANSP, 0.45)


#Returns the largest dataframe according to its columns amount 
largest_df <- function(df1, df2) {
  if (ncol(df1) >= ncol(df2)) {
    return(df1)
  } else {
    return(df2)
  }
}

#Returns a list with dataframes that have a primary key to which they have a functional dependency
#It takes a dataframe and the porcentage of functional dependency we want
dependent.Tables <- function(originalDf, percentage){
  tables_to_be  <- namesOfTablesToBe(originalDf, 0.45)
  # Create a list to store the new data frames
  new_data_frames <- list()
  # Create the new data frames
  for (table_name in names(tables_to_be)) {
    columns_to_get <- rownames(tables_to_be[[table_name]])
    assigned_name <- table_name
    new_df <-  originalDf %>% select(all_of(columns_to_get))
    new_data_frames[[assigned_name]] <- new_df
    for (df_name in setdiff(names(new_data_frames), table_name)) {
      smallest_table_name <- df_name #I set this here, then it'll change with the if it has to
      df <- new_data_frames[[df_name]]
      common_col_names <- intersect(colnames(df), colnames(new_df))
      if (ncol(df) >= ncol(new_df)) {
        largest_table <- df
        largest_table_name <- df_name
        smallest_table_name <- table_name
      } else {
        largest_table <- new_df
        largest_table_name <- table_name
      }
      largest_table <- largest_table[, !colnames(largest_table) %in% common_col_names]
      # Add the column with smallest_table_name back to largest_table (so it's then used as a FK)
      largest_table[[smallest_table_name]] <- df[[smallest_table_name]]
      new_data_frames[[largest_table_name]] <- largest_table
    }
  }
  return(new_data_frames)
}
# Running example:
# new_data_frames <- dependent.Tables(LOANSP, 0.45)






