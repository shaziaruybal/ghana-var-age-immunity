# PREVALENCE

# prev_data <- read.csv("data/infection_prevalence_sourcedata.csv")
prev_data <- readRDS("data/infection_prevalence_sourcedata.rds")

prev_title <- expression(paste(italic("P. falciparum"), " prevalence"))

p_ews_prev <- prev_data %>% filter(season == "End of wet season") %>% 
  ggplot(aes(x = as.factor(AgeGroups), y = n, fill = as.factor(InfectionState))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.75) + 
  scale_fill_manual(values = c("transparent", "#bde1dd", "#7ac3bc"), labels = c("", "Submicroscopic", "Microscopic")) +
  scale_x_discrete(labels = c("1-5 years", "6-10 years", "11-20 years", "21-39 years", "≥40 years")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Age group", 
    y = prev_title,
    fill = "") +
  theme_cowplot() +
  theme(text = element_text(size = 12, family = "sans"),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.95)) +
  background_grid(major = "xy", minor = "none") 

p_eds_prev <- prev_data %>% filter(season == "End of dry season") %>% 
  ggplot(aes(x = as.factor(AgeGroups), y = n, fill = as.factor(InfectionState))) + 
  geom_bar(stat = "identity", position = "fill", width = 0.75) + 
  scale_fill_manual(values = c("transparent", "#efe0c1", "#dfc283"), labels = c("", "Submicroscopic", "Microscopic")) +
  scale_x_discrete(labels = c("1-5 years", "6-10 years", "11-20 years", "21-39 years", "≥40 years")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Age group", 
    y = prev_title,
    fill = "") +
  theme_cowplot() +
  theme(text = element_text(size = 12, family = "sans"),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.95)) +
  background_grid(major = "xy", minor = "none")

p_ews_prev + p_eds_prev + plot_annotation(tag_level = "A") 
ggsave("viz_final/IJPara21_MM04_R1_figureS1A-B.tiff", width = 12, dpi = 300)

# pointprev_data <- prev_data %>% group_by(AgeGroups, season) %>% summarise(total = sum(n))
# 
# prev_data %>% 
#   left_join(pointprev_data, by = c("AgeGroups", "season")) %>% 
#   mutate(pointprev = n/total) %>% 
#   mutate(AgeGroups = factor(AgeGroups, 
#                             levels = c("1-5 years", "6-10 years", "11-20 years", "21-39 years", ">39 years"))) %>% 
#   filter(InfectionState != "Negative") %>% 
#   ggplot(aes(x = AgeGroups, y = pointprev, group = InfectionState, color = InfectionState)) +
#     geom_point() +
#     geom_line() +
#     geom_smooth(method = "gam") +
#     #scale_color_manual(values = c("transparent", "#efe0c1", "#dfc283"), labels = c("", "Submicroscopic", "Microscopic")) +
#     facet_wrap(~season)
