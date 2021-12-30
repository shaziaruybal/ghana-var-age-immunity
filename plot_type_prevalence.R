library(tidyverse)
library(cowplot)
library(patchwork)

# type_prev_season <- read.csv("data/type_prevalence_season_sourcedata.csv")
# type_prev_age <- read.csv("data/type_prevalence_age_sourcedata.csv")

type_prev_season <- readRDS("data/type_prevalence_season_sourcedata.rds")
type_prev_age <- readRDS("data/type_prevalence_age_sourcedata.rds")

(type_prev_season %>% 
  mutate(Season = factor(Season, levels = c("End of dry season", "Found in both seasons", "End of wet season"))) %>% 
  mutate(Ups = factor(Ups, levels = c("non-upsA DBLα types", "upsA DBLα types", "All DBLα types"))) %>% 
  ggplot(aes(x = Ups, y = n, fill = Season)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("#d8b365", "#636363", "#5ab4ac")) +
  scale_x_discrete(labels = c("non-upsA DBLα types \n(N=40,016)", "upsA DBLα types \n(N=2,383)", "All DBLα types \n(N=42,399)")) +
  labs(
    x = "",
    y = "Proportion",
    fill = "") +
  coord_flip() +
  background_grid(major = "xy", minor = "none") +
  theme_cowplot() +
  theme(legend.position = "bottom")) /

(type_prev_age %>% 
  mutate(Season = factor(Season, levels = c("Not found in either season", "End of dry season", "Found in both seasons", "End of wet season"))) %>% 
  mutate(AgeGroupCollapsed = factor(AgeGroupCollapsed, levels = c("Adults (>20 years)", "Adolescents (11-20 years)", "Children (1-10 years)"))) %>% 
  mutate(Ups = factor(Ups, levels = c("All types", "upsA", "non-upsA"))) %>% 
  ggplot(aes(x = factor(AgeGroupCollapsed), y = n, fill = factor(Season))) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("transparent", "#d8b365", "#636363", "#5ab4ac")) + 
  background_grid(major = "xy", minor = "none") + 
  labs(
    x = "Age group", 
    y = "Proportion") +
  coord_flip() + 
  facet_grid(Ups~.) +
  theme_cowplot() +
  theme(legend.position = "none") +
  background_grid(major = "xy", minor = "none")) +
  
  plot_annotation(tag_levels = "a") + plot_layout(heights = c(1, 1.5)) 

theme_set(theme_cowplot(font_size = 12))
ggsave("viz/type_prevalence.png", width = 10, height = 8)