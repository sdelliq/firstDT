#Pieces of code I'll most likely never use but refuse to delete

# CTRL SHIFT C ALLOWS TO COMMENT A BLOCK OF TEXT

#Create a function to see which columns might have multiple values
possMultVals <- function(dataframe){
  names <- colnames(dataframe)
  for (colN in 1:ncol(dataframe)){
    columnV <- as.list(unlist(colN))
    for(rowN in 1:length(columnV)){
      if (grepl("-", dataframe[colN], fixed = TRUE)){
        print(paste("There's a - in", names[colN]))
      }
      if(grepl("/", dataframe[colN], fixed = TRUE)){
        print(paste("There's a / in", names[colN]))
      }
      # if(length(str_locate_all(as.character(dataframe[colN]), ",") [[1]]) > 0){
      #   print(paste("There's a , in", names[colN]))
      # }
      else(
        print(paste("Nothing weird in", names[colN]))
      )
    }
  }
}
possMultVals(NDG)


for (i in colnames(df)) {
  groupvar <- enquo(i)
  df.i <- df %>%
    group_by(.data[[i]])
}







#I'm missing the NDG unique identifier
fromStart <- 100
toFinish <- fromStart + nrow(NDG) - 1
NDG <- NDG %>% mutate(NDG_ID = fromStart:toFinish)
NDG <- NDG %>% relocate(NDG_ID, .before= NDG)