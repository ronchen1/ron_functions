GetTotalAbundanceAndRichness <- function(Tlong,vars_left,var_right = "species_latin",var_value = "Total") {
  library(dplyr)
  Twide <- cast(Tlong,paste0(paste0(vars,collapse = " + ")," ~ ",var_right), fun.aggregate = sum, value = var_value) %>% 
    as_tibble()
  spec_ind <- seq(length(vars)+1,ncol(Twide))
  Twide %>% mutate(total_abund = rowSums(Twide[,spec_ind]))
  Twide %>% mutate(total_richness = rowSums(Twide[,spec_ind]>0))
  return(Twide)
}