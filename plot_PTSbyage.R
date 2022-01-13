library(tidyverse)
library(cowplot)
library(patchwork)

##########################
##Get data
##########################
# EWS_all <- read.csv("data/processed/PTSall_EWS_sourcedata.csv")
# EDS_all <- read.csv("data/processed/PTSall_EDS_sourcedata.csv")
# temporal_all <- read.csv("data/processed/PTSall_Temporal_sourcedata.csv")
# 
# EWS_age <- read.csv("data/processed/PTSage_EWS_sourcedata.csv")
# EDS_age <- read.csv("data/processed/PTSage_EDS_sourcedata.csv")
# temporal_age <- read.csv("data/processed/PTSage_Temporal_sourcedata.csv")

EWS_all <- readRDS("data/processed/PTSall_EWS_sourcedata.rds")
EDS_all <- readRDS("data/processed/PTSall_EDS_sourcedata.rds")
temporal_all <- readRDS("data/processed/PTSall_Temporal_sourcedata.rds")

EWS_age <- readRDS("data/processed/PTSage_EWS_sourcedata.rds")
EDS_age <- readRDS("data/processed/PTSage_EDS_sourcedata.rds")
temporal_age <- readRDS("data/processed/PTSage_Temporal_sourcedata.rds")

##########################
##Curation
##########################
### add a category for the population
EWS_all <- EWS_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
EWS_data <- rbind(EWS_age, EWS_all) 

EDS_all <- EDS_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
EDS_data <- rbind(EDS_all, EDS_age) 

temporal_all <- temporal_all %>% mutate(AgeSimple1 = "All") %>% mutate(AgeSimple2 = "All")
temporal_data <- rbind(temporal_all, temporal_age) 
  
### re-level age classes
EWS_data <- EWS_data %>% mutate(AgeSimple1 = factor(AgeSimple1, 
                                                    levels = c("All", 
                                                               "Children (1-10 years)", 
                                                               "Adolescents (11-20 years)", 
                                                               "Adults (>20 years)")))
EWS_data <- EWS_data %>% mutate(AgeSimple2 = factor(AgeSimple2, 
                                                    levels = c("All",
                                                               "Children (1-10 years)", 
                                                               "Adolescents (11-20 years)", 
                                                               "Adults (>20 years)")))

EDS_data <- EDS_data %>% mutate(AgeSimple1 = factor(AgeSimple1, 
                                                    levels = c("All",
                                                               "Children (1-10 years)", 
                                                               "Adolescents (11-20 years)", 
                                                               "Adults (>20 years)")))
EDS_data <- EDS_data %>% mutate(AgeSimple2 = factor(AgeSimple2, 
                                                    levels = c("All",
                                                               "Children (1-10 years)", 
                                                               "Adolescents (11-20 years)", 
                                                               "Adults (>20 years)")))

temporal_data <- temporal_data %>% mutate(AgeSimple1 = factor(AgeSimple1, 
                                                              levels = c("All",
                                                                         "Children (1-10 years)", 
                                                                         "Adolescents (11-20 years)", 
                                                                         "Adults (>20 years)")))
temporal_data <- temporal_data %>% mutate(AgeSimple2 = factor(AgeSimple2, 
                                                              levels = c("All",
                                                                         "Children (1-10 years)", 
                                                                         "Adolescents (11-20 years)", 
                                                                         "Adults (>20 years)")))

##########################
##Plot 0-1
##########################
title <- expression(paste("Repertoire relatedness (P"["TS"], ")"))

p_PTSwet <- EWS_data %>% 
  ggplot(aes(x = factor(AgeSimple1), y = PTS_score)) +
    geom_violin(fill = "#5ab4ac") +
    geom_hline(yintercept = median(EWS_data[EWS_data$AgeSimple1=="Children (1-10 years)", ]$PTS_score), 
               linetype = "dashed") +
    scale_x_discrete(labels = c("All ages",
                                "Children \n(1-10 years)", 
                                "Adolescents \n(11-20 years)", 
                                "Adults \n(>20 years)")) +
    scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
    labs(
      x = "", 
      y = title) +
    theme_cowplot() +
    theme(
      text = element_text(size = 12, family = "sans"),
      legend.position = "none") +
    geom_boxplot(width = 0.15, fill = "white") +
    background_grid(major = "xy", minor = "none")

p_PTSdry <- EDS_data %>% 
  ggplot(aes(x = factor(AgeSimple1), y = PTS_score)) +
   geom_violin(fill = "#d8b365") + 
   geom_hline(yintercept = median(EDS_data[EDS_data$AgeSimple1=="Children (1-10 years)", ]$PTS_score), 
              linetype = "dashed") +
   labs(
     x = "", 
     y = title) +
   scale_x_discrete(labels = c("All ages",
                               "Children \n(1-10 years)", 
                               "Adolescents \n(11-20 years)", 
                               "Adults \n(>20 years)")) +
   scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  theme_cowplot() +
  theme(
     text = element_text(size = 12, family = "sans"),
     legend.position = "none") +
   geom_boxplot(width = 0.15, fill = "white") + 
   background_grid(major = "xy", minor = "none")

p_PTStemporal <- temporal_data %>% 
  ggplot(aes(x = factor(AgeSimple1), y = PTS_score)) + 
   geom_violin(fill = "#808080") +
   geom_hline(yintercept = median(temporal_data[temporal_data$AgeSimple1=="Children (1-10 years)", ]$PTS_score), 
              linetype = "dashed") +
   labs(
     x = "", 
     y = title) +
   scale_x_discrete(labels = c("All ages",
                               "Children \n(1-10 years)", 
                               "Adolescents \n(11-20 years)", 
                               "Adults \n(>20 years)")) +
   scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
  theme_cowplot() +
  theme(
     text = element_text(size = 12, family = "sans"),
     legend.position = "none") +
   geom_boxplot(width = 0.15, fill = "white") + 
   background_grid(major = "xy", minor = "none") 
  
##########################
##Plot zoom in 0-0.15
##########################
p_PTSwet_zoom <- EWS_data %>% 
  filter(AgeSimple1 != "All" | AgeSimple2 != "All") %>% 
  ggplot(aes(x = PTS_score, fill = AgeSimple1)) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("#2d5a56", "#9cd2cd", "#def0ee")) +
    scale_y_continuous(limits = c(0, 35)) +
    scale_x_continuous(limits = c(0, 0.15)) +
    labs(
      x = title,
      y = "Kernal density",
      fill = "") +
    theme_cowplot() +
    theme(
      text = element_text(size = 12, family = "sans"),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top")) +
    background_grid(major = "xy", minor = "none")
 
p_PTSdry_zoom <- EDS_data %>% 
  filter(AgeSimple1 != "All" | AgeSimple2 != "All") %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = PTS_score, fill = AgeSimple1)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("#6c5932", "#e7d1a2", "#f7efe0")) +  
  scale_y_continuous(limits = c(0, 35)) +
  scale_x_continuous(limits = c(0, 0.15)) +
  labs(
    x = title,
    y = "Kernal density",
    fill = "") +
  theme_cowplot() +
  theme(
    text = element_text(size = 12, family = "sans"),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top")) +
  background_grid(major = "xy", minor = "none")

p_PTStemporal_zoom <- temporal_data %>% 
  filter(AgeSimple1 != "All" | AgeSimple2 != "All") %>% 
  mutate(AgeSimple1 = factor(AgeSimple1, levels = c("Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  ggplot(aes(x = PTS_score, fill = AgeSimple1)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("#0c0c0c", "#b2b2b2", "white")) +  
  scale_y_continuous(limits = c(0, 35)) +
  scale_x_continuous(limits = c(0, 0.15)) +
  labs(
    x = title,
    y = "Kernal density",
    fill = "") +
  theme_cowplot() +
  theme(
    text = element_text(size = 12, family = "sans"),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top")) +
  background_grid(major = "xy", minor = "none")

p_PTSwet + p_PTSdry + p_PTStemporal + p_PTSwet_zoom + p_PTSdry_zoom + p_PTStemporal_zoom + plot_annotation(tag_levels = "A") 

#################
##Save plot 
#################
theme_set(theme_cowplot(font_size = 10))
# ggsave("viz/PTS_season_age_v2.png", width = 16, height = 8)
ggsave("viz_final/IJPara21_MM04_R1_figure2.tiff", width = 16, height = 8)

#################
##Stats
#################
#EWS
EWS_data %>% group_by(AgeSimple1) %>% count()
kruskal.test(PTS_score ~ AgeSimple1, data = EWS_data)
pairwise.wilcox.test(EWS_data$PTS_score, EWS_data$AgeSimple1, p.adjust.method = "bonferroni")

#EDS
EDS_data %>% group_by(AgeSimple1) %>% count()
kruskal.test(PTS_score ~ AgeSimple1, data = EDS_data)
pairwise.wilcox.test(EDS_data$PTS_score, EDS_data$AgeSimple1, p.adjust.method = "bonferroni")

#Temporal
temporal_data %>% group_by(AgeSimple1) %>% count()
kruskal.test(PTS_score ~ AgeSimple1, data = temporal_data)
pairwise.wilcox.test(temporal_data$PTS_score, temporal_data$AgeSimple1, p.adjust.method = "bonferroni")
