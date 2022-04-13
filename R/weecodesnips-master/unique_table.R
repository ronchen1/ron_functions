## Functions to create a table (not data frame) of unique values in each column. Values of the same row do not have any relation!

# Install required packages -----------------------------------------------

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse)

# Convert list of lists into a dataframe ----------------------------------
## Assiting function

list_to_df <- function(ls){
  indx <- lengths(ls) 
  res <- as.data.frame(do.call(rbind, lapply(ls, `length<-`, max(indx))))
  
  colnames(res) <- names(ls[[which.max(indx)]])
  res
}

# Create a table of unique values for each column -------------------------

unique_table <- function(data){
  sapply(data, function(x) unique(x[!is.na(x)] %>% sort())) %>%
    list_to_df() %>% t() %>% dplyr::as_tibble() %>% View("Unique Table")
}
