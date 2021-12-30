library(tidyverse)
library(ggpubr)
library(cowplot)

# repsize_data <- read.csv("data/processed/repertoire_size_sourcedata.csv")
repsize_data <- readRDS("data/processed/repertoire_size_sourcedata.rds")

y_title <- expression(paste0("MOI"[italic("var")]))

repsize_data %>%
  mutate(AgeGroup = factor(AgeGroup, levels = c("Children (1-10 years)", "Adolescents (11-20 years)", "Adults (>20 years)"))) %>% 
  mutate(Season = factor(Season, levels = c("End of wet season", "End of dry season"))) %>% 
  ggplot(aes(x = AgeGroup, y = var_MOI)) +   
  geom_boxplot(aes(fill = factor(Season))) + 
  # annotate(geom = "rect", xmin = 0, xmax = 4, ymin = 45, ymax = 70,
  #          fill = "lightgrey", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_boxplot(aes(fill = factor(Season))) + 
  scale_fill_manual(
    values = c("#5ab4ac", "#d8b365"), 
    name = "") + 
  theme(
    text = element_text(size = 12, family="sans"),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank()) +
  labs(
    x = "", 
    y = y_title,
    fill = "") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme_cowplot() +
  background_grid(major = "xy", minor = "none")

theme_set(theme_cowplot(font_size = 12))
ggsave("viz/varMOI_upsBC.png", width = 10)

### STATS
#### N's
repsize_data %>% group_by(AgeGroup, ) %>% count()

#### Summary stats
repsize_data %>% group_by(Season, AgeGroup) %>% summarise(min = min(var_MOI),
                                                          med = median(var_MOI),
                                                          avg = mean(var_MOI),
                                                          max = max(var_MOI))

repsize_data %>% group_by(Season, AgeGroup) %>% summarise(max = max(var_MOI))

##### End of wet season
kruskal.test(repertoire_size ~ AgeGroup, data = repsize_data %>% filter(Season == "End of wet season"))
pairwise.wilcox.test(repsize_data[repsize_data$Season=="End of wet season",]$repertoire_size, 
                     repsize_data[repsize_data$Season=="End of wet season",]$AgeGroup, 
                     p.adjust.method = "bonferroni")

##### End of dry season
kruskal.test(repertoire_size ~ AgeGroup, data = repsize_data %>% filter(Season == "End of dry season"))
pairwise.wilcox.test(repsize_data[repsize_data$Season=="End of dry season",]$repertoire_size, 
                     repsize_data[repsize_data$Season=="End of dry season",]$AgeGroup, 
                     p.adjust.method = "bonferroni")