mention_nas <- function(data){
  sapply(data, function(x) sum(is.na(x)))
}
