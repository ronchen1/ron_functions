GetTotalAbundanceAndRichness <- function(Tlong,vars_left,var_right = "species_latin",var_value = "Total") {
  # Install required packages -----------------------------------------------
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyvers)
  
  # cast into wide format and calculate abundance and richness ----
  Twide <- cast(Tlong,paste0(paste0(vars,collapse = " + ")," ~ ",var_right), fun.aggregate = sum, value = var_value) %>% 
    as_tibble()
  spec_ind <- seq(length(vars)+1,ncol(Twide))
  Twide1 <- mutate(Twide, total_abund = rowSums(Twide[,spec_ind]))
  Twide2 <-  mutate(Twide1,total_richness = rowSums(Twide[,spec_ind]>0))
  return(Twide2)
}