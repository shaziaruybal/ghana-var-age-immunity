### FIX STAT_COMPARE_MEANS!!!

##########################
##Get data
##########################
# EWS_ups_all <- read.csv("data/processed/PTSall_ups_EWS_sourcedata.csv")
# EWS_ups_age <- read.csv("data/processed/PTSage_ups_EWS_sourcedata.csv")
#   
# EDS_ups_all <- read.csv("data/processed/PTSall_ups_EDS_sourcedata.csv")
# EDS_ups_age <- read.csv("data/processed/PTSage_ups_EDS_sourcedata.csv")
#   
# Temporal_ups_all <- read.csv("data/processed/PTSall_ups_Temporal_sourcedata.csv")
# Temporal_ups_age <- read.csv("data/processed/PTSage_ups_Temporal_sourcedata.csv")
# 
# paired_ups_data <- read.csv("data/processed/PTSage_ups_paired_sourcedata.csv")

EWS_ups_all <- readRDS("data/processed/PTSall_ups_EWS_sourcedata.rds")
EWS_ups_age <- readRDS("data/processed/PTSage_ups_EWS_sourcedata.rds")

EDS_ups_all <- readRDS("data/processed/PTSall_ups_EDS_sourcedata.rds")
EDS_ups_age <- readRDS("data/processed/PTSage_ups_EDS_sourcedata.rds")

Temporal_ups_all <- readRDS("data/processed/PTSall_ups_Temporal_sourcedata.rds")
Temporal_ups_age <- readRDS("data/processed/PTSage_ups_Temporal_sourcedata.rds")

paired_ups_data <- readRDS("data/processed/PTSage_ups_paired_sourcedata.rds")

##########################
## Curation
##########################
### add a category for the population
EWS_ups_all <- EWS_ups_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
EWS_ups_data <- rbind(EWS_ups_age, EWS_ups_all) 

EDS_ups_all <- EDS_ups_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
EDS_ups_data <- rbind(EDS_ups_age, EDS_ups_all) 

Temporal_ups_all <- Temporal_ups_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
Temporal_ups_data <- rbind(Temporal_ups_age, Temporal_ups_all) 

##########################
##Plot
##########################

p_EWS_ups <- EWS_ups_data %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA"))) %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("All", "Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = AgeSimple1, y = PTS_score, fill = Ups)) + 
  geom_boxplot() + 
  labs(
    x = "", 
    y = "Repertoire Relatedness (PTS)",
    fill = "") +
  scale_fill_manual(labels = c("upsA", "non-upsA"), values = c("#7ac3bc", "#cde8e6")) + 
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_discrete(labels = c("All ages",
                              "Children \n(1-10 years)", 
                              "Adolescents \n(11-20 years)", 
                              "Adults \n(>20 years)")) +
  #stat_compare_means(method = "wilcox.test", label = "p.signif", label.x = 3, label.y = 0.5, size = 3) + 
  theme_cowplot() +
  background_grid(major = "xy", minor = "none")

p_EDS_ups <- EDS_ups_data %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA"))) %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("All", "Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = AgeSimple1, y = PTS_score, fill = Ups)) + 
  geom_boxplot() + 
  labs(
    x = "", 
    y = "Repertoire Relatedness (PTS)",
    fill = "") +
  scale_fill_manual(labels = c("upsA", "non-upsA"), values = c("#dfc283", "#efe0c1")) +  
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_discrete(labels = c("All ages",
                              "Children \n(1-10 years)", 
                              "Adolescents \n(11-20 years)", 
                              "Adults \n(>20 years)")) +
  #stat_compare_means(method = "wilcox.test", label = "p.signif", label.x = 3, label.y = 0.5, size = 3) + 
  theme_cowplot() +
  background_grid(major = "xy", minor = "none")

p_Temporal_ups <- Temporal_ups_data %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA"))) %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("All", "Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = AgeSimple1, y = PTS_score, fill = Ups)) + 
  geom_boxplot() + 
  labs(
    x = "", 
    y = "Repertoire Relatedness (PTS)",
    fill = "") +
  scale_fill_manual(labels = c("upsA", "non-upsA"), values = c("#808080", "#cccccc")) +  
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_discrete(labels = c("All ages",
                              "Children \n(1-10 years)", 
                              "Adolescents \n(11-20 years)", 
                              "Adults \n(>20 years)")) +
  #stat_compare_means(method = "wilcox.test", label = "p.signif", label.x = 3, label.y = 0.5, size = 3) + 
  theme_cowplot() +
  background_grid(major = "xy", minor = "none")

p_paired_ups <- paired_ups_data %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA"))) %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = AgeSimple1, y = PTS_score, fill = Ups)) + 
  geom_boxplot() + 
  labs(
    x = "", 
    y = "Repertoire Relatedness (PTS)",
    fill = "") +
  scale_fill_manual(labels = c("upsA", "non-upsA"), values = c("#9f77b8", "#d5c3e0")) +  
  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  scale_x_discrete(labels = c("Children \n(1-10 years)", 
                              "Adolescents \n(11-20 years)", 
                              "Adults \n(>20 years)")) +
  #stat_compare_means(method = "wilcox.test", label = "p.signif", label.x = 3, label.y = 0.5, size = 3) + 
  theme_cowplot() +
  background_grid(major = "xy", minor = "none")


#################
##Save plot 
#################
(p_EWS_ups + p_EDS_ups) / (p_Temporal_ups + p_paired_ups) + plot_annotation(tag_levels = "a")

theme_set(theme_cowplot(font_size = 10))
ggsave("viz/PTS_season_age.png", width = 16, height = 8)