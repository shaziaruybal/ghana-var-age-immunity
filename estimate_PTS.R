library(data.table)
library(dplyr)
library(tibble)
library(here)

geneticSimilarity <- function(mat){
  newmat <- tcrossprod(mat > 0)
  newmat <- newmat/rowSums(mat > 0)
  return(newmat)  
}

GSmatrixToTable <- function(x){
  diag(x) <- NA
  df <- data.frame(SampleID1 = rownames(x)[row(x)], SampleID2 = colnames(x)[col(x)], PTS_score = c(x))
  df <- na.omit(df)
  return(df)
}

original_matrix <- fread(here::here("data", "binary_matrix.csv"), data.table = FALSE)

#### All types
matrix_alltypes <- original_matrix %>% select(-Ups) %>% column_to_rownames('DBLa_type') %>% as.matrix()
PTSmatrix_alltypes <- geneticSimilarity(t(matrix_alltypes))
PTStable_alltypes <- GSmatrixToTable(PTSmatrix_alltypes) 

#### upsA types
matrix_upsAtypes <- original_matrix %>% filter(Ups == "A") %>% select(-Ups) %>% column_to_rownames('DBLa_type') %>% as.matrix()
PTSmatrix_upsAtypes <- geneticSimilarity(t(matrix_upsAtypes))
PTStable_upsAtypes <- GSmatrixToTable(PTSmatrix_upsAtypes) 

#### non-upsA types
matrix_upsBCtypes <- original_matrix %>% filter(Ups == "BC") %>% select(-Ups) %>% column_to_rownames('DBLa_type') %>% as.matrix()
PTSmatrix_upsBCtypes <- geneticSimilarity(t(matrix_upsBCtypes))
PTStable_upsBCtypes <- GSmatrixToTable(PTSmatrix_upsBCtypes) 


##################
#### Merge epi
##################
original_epi <- fread(here::here("data", "epi_data.csv"), data.table = F)

epi_mod1 <- original_epi %>% rename_at(names(.), function(x) paste0(x, "1"))
epi_mod2 <- original_epi %>% rename_at(names(.), function(x) paste0(x, "2"))

epi_formerge <- bind_cols(epi_mod1, epi_mod2)

PTSEpi_alltypes <- PTStable_alltypes %>% inner_join(select(epi_formerge, ends_with("1")), by = "SampleID1") 
PTSEpi_alltypes <- PTSEpi_alltypes %>% inner_join(select(epi_formerge, ends_with("2")), by = "SampleID2") 

PTSEpi_upsAtypes <- PTStable_upsAtypes %>% inner_join(select(epi_formerge, ends_with("1")), by = "SampleID1") 
PTSEpi_upsAtypes <- PTSEpi_upsAtypes %>% inner_join(select(epi_formerge, ends_with("2")), by = "SampleID2") 

PTSEpi_upsBCtypes <- PTStable_upsBCtypes %>% inner_join(select(epi_formerge, ends_with("1")), by = "SampleID1") 
PTSEpi_upsBCtypes <- PTSEpi_upsBCtypes %>% inner_join(select(epi_formerge, ends_with("2")), by = "SampleID2") 


##################
## Save data files
##################

write.csv(PTSEpi_alltypes, here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), row.names = F)
write.csv(PTSEpi_upsAtypes, here::here("data/processed", "PTSEpi_upsATypes_FINAL.csv"), row.names = F)
write.csv(PTSEpi_upsBCtypes, here::here("data/processed", "PTSEpi_upsBCTypes_FINAL.csv"), row.names = F)
