library(data.table)
library(tidyverse)
library(tidyr)
library(here)

original_matrix <- fread(here::here("data", "binary_matrix.csv"), data.table = F)
original_epi <- read.csv(here::here("data", "epi_data.csv"))

repertoire_size_upsA <- original_matrix %>% filter(Ups == "A") %>% 
                                            select(-Ups) %>% 
                                            column_to_rownames("DBLa_type") %>% 
                                            colSums() %>% 
                                            as.data.frame() %>% 
                                            rename("upsA_size" = ".") %>% 
                                            rownames_to_column("SampleID") 

repertoire_size_upsBC <- original_matrix %>% filter(Ups == "BC") %>% 
                                            select(-Ups) %>% 
                                            column_to_rownames("DBLa_type") %>% 
                                            colSums() %>% 
                                            as.data.frame() %>% 
                                            rename("upsBC_size" = ".") %>% 
                                            rownames_to_column("SampleID") 

repertoire_size <- repertoire_size_upsA %>% left_join(repertoire_size_upsBC, by = "SampleID") %>% 
                                            mutate(repertoire_size = upsA_size + upsBC_size)
  
## infer var MOI based cut-off of 45 upsBC per genome
repertoire_size <- repertoire_size %>% mutate(var_MOI = upsBC_size/45) 

## merge with epi for age classes
repertoire_size <- repertoire_size %>% left_join(original_epi %>% select(SampleID, Season, AgeSimple), by = "SampleID") %>% rename("AgeGroup" = "AgeSimple")

repertoire_size <- repertoire_size %>% select(Season, AgeGroup, repertoire_size, var_MOI)

write.csv(repertoire_size, here::here("data/processed", "repertoire_size_sourcedata.csv"), row.names = F)