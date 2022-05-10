remove_rare_species <- function(Afull,cutoff=4) {
  # remove all species with LESS than cutoff observations for the entire dataset
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr)
  
  spp_abund_full <- Afull %>% group_by(SciName) %>% summarise(total_count=sum(count_individuals))
  spp_abund_no_rare <- spp_abund_full %>% filter(total_count>=cutoff)
  A_no_rare <- Afull %>% filter(SciName %in% spp_abund_no_rare$SciName)
}