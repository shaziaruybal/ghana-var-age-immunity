library(tidyverse)
library(cowplot)
library(patchwork)

# paired_data <- read.csv("data/processed/PTSage_paired_sourcedata.csv")
paired_data <- readRDS("data/processed/PTSage_paired_sourcedata.rds")

paired_data <- paired_data %>% mutate(AgeSimple1 = factor(AgeSimple1, 
                                                          levels = c("Children (1-10 years)", 
                                                                     "Adolescents (11-20 years)", 
                                                                     "Adults (>20 years)")))

title <- expression(paste("Repertoire relatedness (P"["TS"], ")"))

### Ns
paired_data %>% group_by(AgeSimple1) %>% count()

### Plot

p_PTSpaired <- paired_data %>% 
  ggplot(aes(x = factor(AgeSimple1), y = PTS_score)) +
    geom_violin(fill = "#bb9fcd") +
    geom_hline(yintercept = median(paired_data[paired_data$AgeSimple1=="Children (1-10 years)", ]$PTS_score), 
               linetype = "dashed") +
    scale_x_discrete(labels = c("Children \n(1-10 years)", 
                                "Adolescents \n(11-20 years)", 
                                "Adults \n(>20 years)")) +
    scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 5)) + 
    labs(
      x = "", 
      y = title) +
    theme(
      text = element_text(size = 12, family = "sans"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none") +
    geom_boxplot(width = 0.15, fill = "white") +
    theme_cowplot() +
    background_grid(major = "xy", minor = "none")

p_PTSpaired_zoom <- paired_data %>% 
ggplot(aes(x = PTS_score, fill = AgeSimple1)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("#756bb1", "#bcbddc", "#efedf5")) +
  # scale_fill_manual(values = c("#4A3F52", "#A88FB8", "#E3D8EB")) +  
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

p_PTSpaired + p_PTSpaired_zoom + plot_annotation(tag_levels = "a")
#################
##Save plot 
#################
theme_set(theme_cowplot(font_size = 10))
ggsave("viz/PTS_paired_samples_revision.png", width = 10, height = 5)
