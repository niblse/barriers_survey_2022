# Create "Carnegie category frequency of barriers" figure for Survey 2
# modeled on Figure 4 of Williams et al. (2019) using figure_04.R

# prerequisites
rm(list = ls())
require(tidyverse)
require(ggthemes)
require(infer)

# start with the `Merged_Data` set produced in Chapter 0 and rename columns
Merged_Data <- read_csv("Merged_Data_Anonymous.csv")

# Select BASIC2018_bins_text.Current & "I lack" level of challenge columns, replace names & make data tidy
Challenge_df <- Merged_Data %>% 
  select(BASIC2018_bins_text.Current, 
         `I lack expertise in bioinformatics`= `I lack expertise in bioinformatics.`,
         `I lack experience teaching bioinformatics` = `I lack experience in teaching bioinformatics....22`,
         `I lack time to restructure course(s)` = `I lack time to restructure course(s).`,
         `I lack autonomy to add course content` = `I lack the autonomy to add content to my course(s)....24`,
         `I lack space to add course content` = `I lack space in my course(s) to add content....25`,
         `I lack curricular materials` = `I lack curricular materials....26`,
         `I lack technical resources` = `I lack appropriate technical resources (internet access/software/hardware/IT support)....27`,
         `My students lack technical resources` = `My student population lacks access to appropriate technical resources (internet access/software/hardware/IT support)....28`,
         `My students lack prerequisite skills` = `My student population lacks prerequisite skills`,
         `My students lack interest` = `My student population lacks interest in bioinformatics....30`) %>% 
  pivot_longer(-BASIC2018_bins_text.Current, names_to = "Barrier", values_to = "Level of Challenge") %>% 
  mutate(`Level of Challenge` = replace_na(`Level of Challenge`, "Not a challenge")) %>% 
  mutate(`Level of Challenge` = factor(`Level of Challenge`, 
                                       levels = c("Severe challenge", "Moderate challenge", 
                                                  "Minor challenge", "Not a challenge")
  ))

# conduct test for association between Carnegie classification and each barrier
# get list of Carnegie classifications with >50 responses
list_freq_Cc <- Merged_Data %>% 
  count(BASIC2018_bins_text.Current) %>% 
  filter(n > 50) %>% 
  pull(BASIC2018_bins_text.Current)
  
# 
test_results <- unique(Challenge_df$Barrier) %>%
  map_df(function(x) { 
    Challenge_df %>% 
      filter(BASIC2018_bins_text.Current %in% list_freq_Cc) %>% 
      filter(Barrier == x) %>% 
      chisq_test(`Level of Challenge` ~ BASIC2018_bins_text.Current)
  }
  )  

test_results <- test_results %>% 
  mutate(Barrier = unique(Challenge_df$Barrier),
         adj.p_val = p.adjust(p_value, method = "fdr")
  ) %>% 
  select(Barrier, everything()) %>% 
  mutate(`adj. p-val` = if_else(adj.p_val < 0.01, 
                                paste0("adj. p-val = ", as.character(signif(adj.p_val, 2))),
                                ""))

sig_results <- test_results %>% filter(adj.p_val < 0.01)

# plot challenge levels for each barrier that differs significantly among genders
# list of barriers that differ significantly
sig_barriers <- sig_results %>% pull(Barrier)

# plot challenge level percentages for each Cc-significant barrier
challenge_percents <- Challenge_df %>%
  filter(BASIC2018_bins_text.Current %in% list_freq_Cc) %>% 
  filter(Barrier %in% sig_barriers) %>% 
  group_by(Barrier, BASIC2018_bins_text.Current) %>% 
  count(`Level of Challenge`, name = "count") %>% 
  mutate(proportion = (count/sum(count)))

legend.labels <- Merged_Data %>% 
  filter(BASIC2018_bins_text.Current %in% list_freq_Cc) %>% 
  count(BASIC2018_bins_text.Current) %>% 
  mutate(label = paste0(BASIC2018_bins_text.Current, "\n (n = ", n,")"))

# generate plot with flipped coordinates, reordering variables
greys <- c("#595959", 
           "#778899", 
           "#a6a6a6", 
           "#ededed")
# # "#DCDCDC")
# 
# 
# # plot with `Level of Challenge` fill
# challenge_percents %>%
#   ggplot(aes(x=factor(BASIC2018_bins_text.Current, 
#                       levels = c("Associate's Colleges",
#                                  "Baccalaureate Colleges",
#                                  "Master's Colleges & Universities",
#                                  "Doctoral/Professional Universities"),
#                       labels = c("Associate's\n (n = 66)", 
#                                  "Baccalaureate\n (n = 77)", 
#                                  "Master's\n (n = 115)", 
#                                  "Doctoral\n (n = 232)")),
#              y=proportion, fill=`Level of Challenge`)) +
#   geom_bar(position = "dodge", stat = "Identity")+
#   labs(y = "percentage of respondents", x= "")+
#   facet_wrap(vars(factor(Barrier)),
#              scales = "free_y", ncol = 2) +
#   scale_y_continuous(labels = scales::percent)  +
#   theme_gray(base_size = 20, base_family = "sans") +
#   theme(line = element_line(colour = "black"), 
#         rect = element_rect(fill = "white", linetype = 0, colour = NA))+
#   theme(legend.background = element_rect(), 
#         legend.position = "bottom",
#         legend.title = element_blank()) +
#   theme(panel.grid.major =
#           element_line(colour = "grey"),
#         panel.grid.minor = element_blank(),
#         # unfortunately, can't mimic subtitles
#         plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
#         plot.margin = unit(c(1, 1, 1, 1), "lines"),
#         strip.background = element_rect())+
#   theme(axis.title.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   theme(strip.text.x = element_text(size = 18, face = "bold"))+
#   theme(plot.background = element_rect(fill = "white"))+
#   theme(panel.background = element_rect(fill = "white"))+
#   theme(panel.grid.major.y = element_blank())+
#   theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
#   theme(axis.text = element_text(size = 18)) +
#   coord_flip()+
#   scale_fill_manual(values = greys, labels= unique(challenge_percents$`Level of Challenge`))+
#   guides(fill=guide_legend(reverse = TRUE))+
#   theme(panel.grid.minor=element_blank())
# 
# # ggsave() the last plot displayed
# ggsave("figure_04_survey2.png", 
#        units = "in", 
#        height = 8, 
#        width = 12)                               

# stacked bar plot with `Level of Challenge` fill
challenge_percents %>%
  ggplot(aes(x=factor(BASIC2018_bins_text.Current, 
                      levels = c("Associate's Colleges",
                                 "Baccalaureate Colleges",
                                 "Master's Colleges & Universities",
                                 "Doctoral/Professional Universities"),
                      labels = c("Associate's\n (n = 66)", 
                                 "Baccalaureate\n (n = 77)", 
                                 "Master's\n (n = 115)", 
                                 "Doctoral\n (n = 232)")),
             y=proportion, fill=`Level of Challenge`)) +
  geom_bar(# position = "dodge", 
           stat = "Identity")+
  labs(y = "percentage of respondents", x= "")+
  facet_wrap(vars(factor(Barrier)),
             scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::percent)  +
  theme_gray(base_size = 20, base_family = "sans") +
  theme(line = element_line(colour = "black"), 
        rect = element_rect(fill = "white", linetype = 0, colour = NA))+
  theme(legend.background = element_rect(), 
        legend.position = "bottom",
        legend.title = element_blank()) +
  theme(panel.grid.major =
          element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        # unfortunately, can't mimic subtitles
        plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        strip.background = element_rect())+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
  theme(axis.text = element_text(size = 18)) +
  coord_flip()+
  scale_fill_manual(values = greys, labels= unique(challenge_percents$`Level of Challenge`))+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(panel.grid.minor=element_blank())

# ggsave() the last plot displayed
ggsave("figure_04alt_survey2.png", 
       units = "in", 
       height = 6, 
       width = 14)                               
