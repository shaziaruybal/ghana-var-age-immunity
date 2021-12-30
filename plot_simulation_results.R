library(ggplot2)
library(data.table)
library(tidyverse)
library(cowplot)

# getRuns <- function(Ups, iterations){
#   lower_curve <- upper_curve <- c()
#   for (i in 1:iterations){
#     print(i)
#     lower_curve <- rbind(lower_curve, fread(paste(results.folder, 'curveData_',Ups,'_0.95_subsampleT_96_run_',i,'.csv',sep=''), he=T))
#     upper_curve <- rbind(upper_curve, fread(paste(results.folder, 'curveData_',Ups,'_0.95_subsampleF_96_run_',i,'.csv',sep=''), he=T))
#   }
#   lower_curve$bound <- 'L'
#   upper_curve$bound <- 'U'
#   curveData <- rbind(lower_curve,upper_curve)
#   return(curveData)
# }
# 
# 
# #################
# ##Get data
# #################
# results.folder <- "./data/results_100iterations/"
# 
# Ups <- c('A','BC','ABC')
# iterations <- 100
# 
# d <- NULL
# for (u in (Ups)){
#     x <- getRuns(u, iterations)
#     d <- rbind(d, x)
#   }

#################
## Get data
#################
d <- readRDS(here::here("data", "results_100iterations.rds"))

#################
##Plot curves
#################
EIR = 25

plot_immunitycurves <- d %>% 
  mutate(Ups = case_when(Ups == "A" ~ "upsA",
                         Ups == "BC" ~ "non-upsA",
                         Ups == "ABC" ~ "All types")) %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA", "All types"))) %>% 
  ggplot(aes(bites/EIR, propVars, color = subsample, group = subsample)) +
    geom_smooth(method = "gam", formula = y ~ s(log(x + 1)), se = T) +
    scale_color_manual(values = c('#9b3773','#479e6f'), #values = c('#9b3773','#e37c37'), #values = c('#9b3773','#479e6f'), values = c('#dc097c','#479e6f'), 
                       name = "Co-transmission", 
                       breaks = c("FALSE", "TRUE"), 
                       labels = c("Yes", "No")) +
    labs(x = 'Age (years)', 
         y = '% immunity to DBLα types') +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 25)) +
    scale_y_continuous(limits = c(0,0.95), breaks = scales::pretty_breaks(n = 10), labels = scales::percent_format(1)) +
    theme(legend.position = "bottom", legend.justification = "center") +
    facet_grid(~Ups) +
    theme_bw() 

#################
##Save curves plot 
#################
theme_set(theme_cowplot(font_size = 10))
ggsave("viz/simulations_immunity.png", width = 10)

#################
##Plot bites
#################
plot_bites_half <- d %>% 
  group_by(run, Ups, subsample) %>% 
  filter(propVars > 0.5) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(Ups = case_when(Ups == "A" ~ "upsA",
                         Ups == "BC" ~ "non-upsA",
                         Ups == "ABC" ~ "All types")) %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA", "All types"))) %>% 
  ggplot(aes(x = subsample, y = bites, fill = subsample)) + 
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = c('#9b3773','#479e6f'), #values = c('#9b3773','#e37c37'), #values = c('#9b3773','#479e6f'), values = c('#dc097c','#479e6f'), 
                      name = "Co-transmission", 
                      breaks = c("FALSE", "TRUE"), 
                      labels = c("Yes", "No")) +
    scale_x_discrete(labels = c("Yes", "No")) + #scale_x_discrete(labels = c("upsA", "non-upsA", "All types")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(x = "",
         y = "Number of infective bites",
         title = "Minimum number of infective bites to acquire immunity to 50% of types") +
    theme_bw() + 
  facet_grid(~Ups) 

plot_bites_all <-d %>% 
  group_by(run, Ups, subsample) %>% 
  filter(propVars > 0.95) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(Ups = case_when(Ups == "A" ~ "upsA",
                         Ups == "BC" ~ "non-upsA",
                         Ups == "ABC" ~ "All types")) %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA", "All types"))) %>% 
  ggplot(aes(x = subsample, y = bites, fill = subsample)) + 
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c('#9b3773','#479e6f'), #values = c('#9b3773','#e37c37'), #values = c('#9b3773','#479e6f'), values = c('#dc097c','#479e6f'), 
                    name = "Co-transmission", 
                    breaks = c("FALSE", "TRUE"), 
                    labels = c("Yes", "No")) +
  scale_x_discrete(labels = c("Yes", "No")) + #scale_x_discrete(labels = c("upsA", "non-upsA", "All types")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "",
       y = "Number of infective bites",
       title = "Minimum number of infective bites to acquire immunity to 95% of types") +
  theme_bw() + 
  facet_grid(~Ups) 

#################
##Save bites plot 
#################
plot_bites_half + plot_bites_all + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect")
theme_set(theme_cowplot(font_size = 10))
ggsave("viz/simulations_bites.png", width = 16)

#################
##Save all panels
#################
(plot_bites_half + plot_bites_all) / plot_immunitycurves + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect")
theme_set(theme_cowplot(font_size = 10))
ggsave("viz/results_simulations.png", width = 16, height = 10)

####################
##Graphical abstract
####################
d %>% 
  mutate(Ups = case_when(Ups == "A" ~ "upsA",
                         Ups == "BC" ~ "non-upsA",
                         Ups == "ABC" ~ "All types")) %>% 
  mutate(Ups = factor(Ups, levels = c("upsA", "non-upsA", "All types"))) %>% 
  filter(Ups == "All types") %>% 
  ggplot(aes(bites/EIR, propVars, color = subsample, group = subsample)) +
  geom_smooth(method = "gam", formula = y ~ s(log(x + 1)), se = T) +
  scale_color_manual(values = c('#9b3773','#479e6f'), #values = c('#9b3773','#e37c37'), #values = c('#9b3773','#479e6f'), values = c('#dc097c','#479e6f'), 
                     name = "Co-transmission", 
                     breaks = c("FALSE", "TRUE"), 
                     labels = c("Yes", "No")) +
  labs(x = 'Age (years)', 
       y = '% immunity to DBLα types') +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 25)) +
  scale_y_continuous(limits = c(0,0.95), breaks = scales::pretty_breaks(n = 10), labels = scales::percent_format(1)) +
  theme(legend.position = "bottom", legend.justification = "center") +
  facet_grid(~Ups) +
  theme_bw() 

theme_set(theme_cowplot(font_size = 10))
ggsave("viz/plot_for_graphical_abstract.png", width = 10)
