unique_table <- function(data){
  list_to_df <- function(ls){
    indx <- lengths(ls) 
    res <- as.data.frame(do.call(rbind, lapply(ls, `length<-`, max(indx))))
    
    colnames(res) <- names(ls[[which.max(indx)]])
    res
  }
  sapply(data, function(x) unique(x[!is.na(x)] %>% sort())) %>%
    list_to_df() %>% t() %>% dplyr::as_tibble() %>% View("Unique Table")
}
