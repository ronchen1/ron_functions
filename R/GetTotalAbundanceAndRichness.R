GetTotalAbundanceAndRichness <- function(Tlong,vars_left,var_right = "species_latin",var_value = "Total") {
  # Install required packages -----------------------------------------------
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)
  
  # cast into wide format and calculate abundance and richness ----
  Twide <- cast(Tlong,paste0(paste0(vars_left,collapse = " + ")," ~ ",var_right), fun.aggregate = sum, value = var_value) %>% 
    as_tibble()
  spec_ind <- seq(length(vars_left)+1,ncol(Twide))
  Twide1 <- mutate(Twide, total_abund = rowSums(Twide[,spec_ind]))
  Twide2 <-  mutate(Twide1,total_richness = rowSums(Twide[,spec_ind]>0))
  Twide_no_species <- cbind(Twide2[,1:length(vars_left)],Twide2[,c("total_abund","total_richness")])
  return(Twide_no_species)
}


SpeciesObs_Long2Wide <- function(Tlong,vars_left,var_right = "species_latin",var_value = "Total") {
  Ttmp <- Tlong %>% select(eval(vars_left))
  Dtmp <- Ttmp %>% pivot_wider(names_from = eval(var_right), values_from = eval(var_value), values_fill=0,
                               values_fn = list(eval(var_value) = sum))
  by_campaign_unit <- group_by(Dtmp,campaign,unit)
  Twide <- by_campaign_unit %>% arrange(point_name, .by_group = TRUE)
  occu <- Twide[,(length(variables)-1):ncol(Twide)]
  outvars <- list(Twide,occu)
}