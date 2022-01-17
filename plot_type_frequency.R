library(tidyverse)
library(cowplot)
library(patchwork)

types <- readRDS("data/DBLa types and Ups 42399.rds")

upsA_types <- types %>% filter(Ups == "A")
upsBC_types <- types %>% filter(Ups == "BC")

ews_types <- types %>% select(Ups, contains("S1M"))
dim(ews_types)

ews_types <- ews_types %>% 
  rowwise() %>% 
  filter(sum(c_across(S1MRS0004.MID01.01.P2.nov13:SBS1MRS2043.MID59.59.P3.dec16)) != 0)
dim(ews_types)

eds_types <- types %>% select(Ups, contains("S2M"))
dim(eds_types)

eds_types <- eds_types %>% 
  rowwise() %>% 
  filter(sum(c_across(S2MRS0004.MID1.1.P1.dec15:SBS2MRS2014.MID7.7.P4.dec16)) != 0)
dim(eds_types)

####################################
## Calculate frequency of each type
####################################
ews_types_freq <- ews_types %>% 
  mutate(frequency = rowSums(across(-Ups))) %>% 
  select(Ups, frequency)

eds_types_freq <- eds_types %>% 
  mutate(frequency = rowSums(across(-Ups))) %>% 
  select(Ups, frequency)

##############
## Plot
##############

p_ews_freq <- ews_types_freq %>% 
  mutate(Ups = case_when(Ups == "A" ~ "upsA (n=2,138)",
                         Ups == "BC" ~ "non-upsA (n=31,479)"),
         Ups = fct_rev(Ups),
         frequency_cat = case_when(frequency >= 10 ~ "≥10",
                                   TRUE~ as.character(frequency))) %>% 
  count(Ups, frequency_cat) %>% 
  group_by(Ups) %>%  
  mutate(proportion = n / sum(n),
         frequency_cat = factor(frequency_cat, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "≥10"))) %>% 
    ggplot(aes(x = frequency_cat, y = proportion, group = Ups, fill = Ups)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(n.breaks = 6, limits = c(0, 0.6)) +
      scale_fill_manual(values = c("#7bb6ae", "#b8dad5")) +
      labs(x = "Frequency",
           y = "Proportion",
           fill = "") +
      theme_cowplot()  +
      background_grid(major = "xy") +
      theme(legend.position = c(0.6, 0.9))

p_eds_freq <- eds_types_freq %>% 
  mutate(Ups = case_when(Ups == "A" ~ "upsA (n=1,801)",
                         Ups == "BC" ~ "non-upsA (n=24,277)"),
         Ups = fct_rev(Ups),
         frequency_cat = case_when(frequency >= 10 ~ "≥10",
                                   TRUE~ as.character(frequency))) %>% 
  count(Ups, frequency_cat) %>% 
  group_by(Ups) %>%  
  mutate(proportion = n / sum(n),
         frequency_cat = factor(frequency_cat, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "≥10"))) %>% 
  ggplot(aes(x = frequency_cat, y = proportion, group = Ups, fill = Ups)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(n.breaks = 6, limits = c(0, 0.6)) +
    scale_fill_manual(values = c("#d2b679", "#e8d9b8")) +
    labs(x = "Frequency",
         y = "Proportion",
         fill = "") +
    theme_cowplot()  +
    background_grid(major = "xy") +
    theme(legend.position = c(0.6, 0.9))

p_ews_freq + p_eds_freq + plot_annotation(tag_levels = "A")
theme_set(theme_cowplot(font_size = 12))
ggsave("viz_final/IJPARA21_MM04_R1_figureS3_final.tiff", width = 12, height = 4)
