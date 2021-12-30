library(data.table)
library(tidyverse)
library(here)

##########################
##Create PTSage source data file
##########################
### EWS
EWS_PTS_temp_data<- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

EWS_PTS_temp_data <- EWS_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
EWS_PTS_temp_data <- EWS_PTS_temp_data %>% filter(Season1 == "End of wet season" & Season2 == "End of wet season", AgeSimple1 == AgeSimple2)

#write.csv(EWS_PTS_temp_data, here::here("data/processed", "PTSage_EWS_sourcedata.csv"), row.names = F)
#saveRDS(EWS_PTS_temp_data, here::here("data/processed", "PTSage_EWS_sourcedata.rds"))

### EDS
EDS_PTS_temp_data <- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

EDS_PTS_temp_data <- EDS_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
EDS_PTS_temp_data <- EDS_PTS_temp_data %>% filter(Season1 == "End of dry season" & Season2 == "End of dry season", AgeSimple1 == AgeSimple2)

#write.csv(EDS_PTS_temp_data, here::here("data/processed", "PTSage_EDS_sourcedata.csv"), row.names = F)
#saveRDS(EDS_PTS_temp_data, here::here("data/processed", "PTSage_EDS_sourcedata.rds"))

### Temporal
Temporal_PTS_temp_data <- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

Temporal_PTS_temp_data <- Temporal_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
Temporal_PTS_temp_data <- Temporal_PTS_temp_data %>% filter((Season1 == "End of wet season" & Season2 == "End of dry season") | (Season1 == "End of dry season" & Season2 == "End of wet season"), AgeSimple1 == AgeSimple2)

#write.csv(Temporal_PTS_temp_data, here::here("data/processed", "PTSage_Temporal_sourcedata.csv"), row.names = F)
#saveRDS(Temporal_PTS_temp_data, here::here("data/processed", "PTSage_Temporal_sourcedata.rds"))

### Paired
Paired_PTS_temp_data <- fread(here::here("data/processed","PTSEpi_AllTypes_FINAL.csv"), data.table = F)

Paired_PTS_temp_data <- Paired_PTS_temp_data %>% mutate(studyID1 = str_replace_all(.$SampleID1, c("\\.MID.*" = "",
                                                                                                  "^(RS|S|SBS)[0-9]" = "")),
                                                        studyID2 = str_replace_all(.$SampleID2, c("\\.MID.*" = "",
                                                                                                  "^(RS|S|SBS)[0-9]" = "")))
Paired_PTS_temp_data <- Paired_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, studyID1, studyID2, PTS_score)
Paired_PTS_temp_data <- Paired_PTS_temp_data %>% filter(studyID1 == studyID2) 

id_list <- data.frame(studyid = unique(Paired_PTS_temp_data$studyID1))
id_list <- id_list %>% arrange(studyid) %>% mutate(id = row_number()) 

Paired_PTS_temp_data <- Paired_PTS_temp_data %>% left_join(id_list, by = c("studyID1" = "studyid"))
Paired_PTS_temp_data <- Paired_PTS_temp_data %>% left_join(id_list, by = c("studyID2" = "studyid"))
Paired_PTS_temp_data <- Paired_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, id.x, id.y, PTS_score) 

#write.csv(Paired_PTS_temp_data, here::here("data/processed", "PTSage_paired_sourcedata.csv"), row.names = F)
#saveRDS(Paired_PTS_temp_data, here::here("data/processed", "PTSage_paired_sourcedata.rds"))

######################################################
##Create PTS source data file all comparisons
######################################################
### EWS
EWS_PTS_temp_data<- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

EWS_PTS_temp_data <- EWS_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
EWS_PTS_temp_data <- EWS_PTS_temp_data %>% filter(Season1 == "End of wet season" & Season2 == "End of wet season")

#write.csv(EWS_PTS_temp_data, here::here("data/processed", "PTSall_EWS_sourcedata.csv"), row.names = F)
#saveRDS(EWS_PTS_temp_data, here::here("data/processed", "PTSall_EWS_sourcedata.rds"))

### EDS
EDS_PTS_temp_data<- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

EDS_PTS_temp_data <- EDS_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
EDS_PTS_temp_data <- EDS_PTS_temp_data %>% filter(Season1 == "End of dry season" & Season2 == "End of dry season")

#write.csv(EDS_PTS_temp_data, here::here("data/processed", "PTSall_EDS_sourcedata.csv"), row.names = F)
#saveRDS(EDS_PTS_temp_data, here::here("data/processed", "PTSall_EDS_sourcedata.rds"))

### Temporal
Temporal_PTS_temp_data<- fread(here::here("data/processed", "PTSEpi_AllTypes_FINAL.csv"), data.table = F)

Temporal_PTS_temp_data <- Temporal_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
Temporal_PTS_temp_data <- Temporal_PTS_temp_data %>% filter((Season1 == "End of wet season" & Season2 == "End of dry season") | (Season1 == "End of dry season" & Season2 == "End of wet season"))

#write.csv(Temporal_PTS_temp_data, here::here("data/processed", "PTSall_Temporal_sourcedata.csv"), row.names = F)
#saveRDS(Temporal_PTS_temp_data, here::here("data/processed", "PTSall_Temporal_sourcedata.rds"))

######################################################
##Create PTS source data file with Ups classification
######################################################

upsA_PTS_temp_data <- read.csv(here::here("data/processed", "PTSEpi_upsATypes_FINAL.csv"))
upsBC_PTS_temp_data <- read.csv(here::here("data/processed", "PTSEpi_upsBCTypes_FINAL.csv"))

upsA_PTS_temp_data <- upsA_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 
upsBC_PTS_temp_data <- upsBC_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, PTS_score) 

upsA_PTS_temp_data <- upsA_PTS_temp_data %>% mutate(Ups = "upsA")
upsBC_PTS_temp_data <- upsBC_PTS_temp_data %>% mutate(Ups = "non-upsA")

ups_PTS_temp_data <- bind_rows(upsA_PTS_temp_data, upsBC_PTS_temp_data)

### EWS all comparisons 
EWS_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter(Season1 == "End of wet season" & Season2 == "End of wet season")
#write.csv(EWS_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_EWS_sourcedata.csv"), row.names = F)
#saveRDS(EWS_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_EWS_sourcedata.rds"))

### EWS age-specific
EWS_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter(Season1 == "End of wet season" & Season2 == "End of wet season", AgeSimple1 == AgeSimple2)
#write.csv(EWS_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_EWS_sourcedata.csv"), row.names = F)
#saveRDS(EWS_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_EWS_sourcedata.rds"))

### EDS all comparisons
EDS_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter(Season1 == "End of dry season" & Season2 == "End of dry season")
#write.csv(EDS_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_EDS_sourcedata.csv"), row.names = F)
#saveRDS(EDS_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_EDS_sourcedata.rds"))

### EDS age-specific
EDS_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter(Season1 == "End of dry season" & Season2 == "End of dry season", AgeSimple1 == AgeSimple2)
#write.csv(EDS_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_EDS_sourcedata.csv"), row.names = F)
#saveRDS(EDS_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_EDS_sourcedata.rds"))

### Temporal all comparisons
Temporal_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter((Season1 == "End of wet season" & Season2 == "End of dry season") | (Season1 == "End of dry season" & Season2 == "End of wet season"))
#write.csv(Temporal_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_Temporal_sourcedata.csv"), row.names = F)
#saveRDS(Temporal_ups_PTS_temp_data, here::here("data/processed", "PTSall_ups_Temporal_sourcedata.rds"))

### Temporal age-specific
Temporal_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter((Season1 == "End of wet season" & Season2 == "End of dry season") | (Season1 == "End of dry season" & Season2 == "End of wet season"), AgeSimple1 == AgeSimple2)
#write.csv(Temporal_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_Temporal_sourcedata.csv"), row.names = F)
#saveRDS(Temporal_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_Temporal_sourcedata.rds"))

### Paired
upsA_PTS_temp_data <- read.csv(here::here("data/processed", "PTSEpi_upsATypes_FINAL.csv"))
upsBC_PTS_temp_data <- read.csv(here::here("data/processed", "PTSEpi_upsBCTypes_FINAL.csv"))

upsA_PTS_temp_data <- upsA_PTS_temp_data %>% mutate(Ups = "upsA")
upsBC_PTS_temp_data <- upsBC_PTS_temp_data %>% mutate(Ups = "non-upsA")

ups_PTS_temp_data <- bind_rows(upsA_PTS_temp_data, upsBC_PTS_temp_data)

ups_PTS_temp_data <- ups_PTS_temp_data %>% mutate(studyID1 = str_replace_all(.$SampleID1, c("\\.MID.*" = "",
                                                                                            "^(RS|S|SBS)[0-9]" = "")),
                                                  studyID2 = str_replace_all(.$SampleID2, c("\\.MID.*" = "",
                                                                                            "^(RS|S|SBS)[0-9]" = "")))

ups_PTS_temp_data <- ups_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, studyID1, studyID2, PTS_score, Ups)
Paired_ups_PTS_temp_data <- ups_PTS_temp_data %>% filter(studyID1 == studyID2) 

id_list <- data.frame(studyid = unique(Paired_ups_PTS_temp_data$studyID1))
id_list <- id_list %>% arrange(studyid) %>% mutate(id = row_number()) 

Paired_ups_PTS_temp_data <- Paired_ups_PTS_temp_data %>% left_join(id_list, by = c("studyID1" = "studyid"))
Paired_ups_PTS_temp_data <- Paired_ups_PTS_temp_data %>% left_join(id_list, by = c("studyID2" = "studyid"))
Paired_ups_PTS_temp_data <- Paired_ups_PTS_temp_data %>% select(Season1, Season2, AgeSimple1, AgeSimple2, id.x, id.y, PTS_score, Ups) 

#write.csv(Paired_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_paired_sourcedata.csv"), row.names = F)
#saveRDS(Paired_ups_PTS_temp_data, here::here("data/processed", "PTSage_ups_paired_sourcedata.rds"))
