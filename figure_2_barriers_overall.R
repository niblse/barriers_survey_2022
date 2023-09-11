# Create "overall frequency of barriers" figure for Survey 2
# modeled on Figure 4 of Williams et al. (2019) using figure_04.R

# prerequisites
rm(list = ls())
require(tidyverse)
require(ggthemes)
require(infer)

# start with the `Merged_Data` set produced in Chapter 0 and rename columns
Merged_Data <- read_csv("Merged_Data_Anonymous.csv")

# Retrieve "I lack" yes/no response columns, replace names & make data tidy
barrier_df <- Merged_Data %>% 
  select(`I lack expertise in bioinformatics`,
         `I lack experience teaching bioinformatics` = `I lack experience in teaching bioinformatics....12`,
         `I lack time to restructure course(s)`,
         `I lack autonomy to add course content` = `I lack the autonomy to add content to my course(s)....14`,
         `I lack space to add course content` = `I lack space in my course(s) to add content....15`,
         `I lack curricular materials` = `I lack curricular materials....16`,
         `I lack technical resources` = `I lack appropriate technical resources (internet access/software/hardware/IT support)....17`,
         `My students lack technical resources` = `My student population lacks access to appropriate technical resources (internet access/software/hardware/IT support)....18`,
         `My students lack prerequisite skills` = `My student population lacks prerequisite skills.`,
         `My students lack interest` = `My student population lacks interest in bioinformatics....20`) %>% 
  pivot_longer(everything(), names_to = "Barrier", values_to = "Response") %>% 
  mutate(Response = replace_na(Response, "I do NOT agree with this statement")) 

# association between barrier and response?
chisq_test(barrier_df, `Response` ~ Barrier)

# which barriers are more often cited?
# calculate overall proportions of each challenge level
barrier_props_df <- barrier_df %>% 
  count(`Response`, name = "count") %>% 
  mutate(proportion = count/sum(count)) 
barrier_props <- barrier_props_df %>% pull()

# conduct goodness of fit test for each barrier
test_results_bar <- unique(barrier_df$Barrier) %>%
  map_df(function(x) { 
    barrier_df %>% 
      filter(Barrier == x) %>% 
      chisq_test(response = Response,
                 p = barrier_props)
  }
  )  
test_results_bar <- test_results_bar %>% 
  mutate(Barrier = unique(barrier_df$Barrier),
         adj.p_val = p.adjust(p_value, method = "fdr")
  ) %>% 
  select(Barrier, everything()) %>% 
  mutate(`adj. p-val` = if_else(adj.p_val < 0.01, "*", "")) 

sig_results_bar <- test_results_bar %>% filter(adj.p_val < 0.01)

# Retrieve "I lack" level of challenge columns, replace names & make data tidy
Challenge_df <- Merged_Data %>% 
  select(`I lack expertise in bioinformatics`= `I lack expertise in bioinformatics.`,
         `I lack experience teaching bioinformatics` = `I lack experience in teaching bioinformatics....22`,
         `I lack time to restructure course(s)` = `I lack time to restructure course(s).`,
         `I lack autonomy to add course content` = `I lack the autonomy to add content to my course(s)....24`,
         `I lack space to add course content` = `I lack space in my course(s) to add content....25`,
         `I lack curricular materials` = `I lack curricular materials....26`,
         `I lack technical resources` = `I lack appropriate technical resources (internet access/software/hardware/IT support)....27`,
         `My students lack technical resources` = `My student population lacks access to appropriate technical resources (internet access/software/hardware/IT support)....28`,
         `My students lack prerequisite skills` = `My student population lacks prerequisite skills`,
         `My students lack interest` = `My student population lacks interest in bioinformatics....30`) %>% 
  pivot_longer(everything(), names_to = "Barrier", values_to = "Level of Challenge") %>% 
  mutate(`Level of Challenge` = replace_na(`Level of Challenge`, "Not a challenge")) %>% 
  mutate(`Level of Challenge` = factor(`Level of Challenge`, 
                                       levels = c("Severe challenge", "Moderate challenge", 
                                                  "Minor challenge", "Not a challenge")
                                       ))

# association between barrier and level challenge?
chisq_test(Challenge_df, `Level of Challenge` ~ Barrier)

# which barriers are significantly greater challenge?
# calculate overall proportions of each challenge level
challenge_props_df <- Challenge_df %>% 
  count(`Level of Challenge`, name = "count") %>% 
  mutate(proportion = count/sum(count)) 
challenge_props <- challenge_props_df %>% pull()

# conduct goodness of fit test for each barrier
test_results <- unique(Challenge_df$Barrier) %>%
  map_df(function(x) { 
    Challenge_df %>% 
      filter(Barrier == x) %>% 
      chisq_test(response = `Level of Challenge`,
                 p = challenge_props)
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

# plot challenge levels for each barrier
# Challenge_df %>% 
#   ggplot(aes(x=`Level of Challenge`)) + 
#   geom_bar() +
#   labs(y = "Number of responses", x= "") +
#   geom_text(data = test_results,
#             mapping = aes(label = `adj. p-val`, x = 4, y = 400), size = 6) +
#   coord_flip() +
#   facet_wrap(vars(Barrier), ncol = 2) +
#   theme_fivethirtyeight(base_size = 20, base_family = "sans") +
#   theme(panel.background = element_rect(fill = "white")) +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme(axis.title.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   theme(strip.text.x = element_text(size = 18, face = "bold"))+
#   theme(axis.line = element_line(colour = "black", 
#                                  linewidth = 0.5, linetype = "solid"))+
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank()) +
#   theme(axis.line.x = element_blank()) +
#   theme(axis.text.y = element_text(hjust = 1.1))

# plot challenge level percentages for all barriers
challenge_percents <- Challenge_df %>%
  group_by(Barrier) %>% 
  count(`Level of Challenge`, name = "count") %>% 
  mutate(proportion = (count/sum(count)))

challenge_percents_Not <- challenge_percents %>% 
  filter(`Level of Challenge` == "Not a challenge") %>% 
  count(Barrier, wt=proportion, name = "Not_pct") %>% 
  arrange(desc(Not_pct))

challenge_percents_ModSev <- challenge_percents %>% 
  filter(`Level of Challenge` %in% c("Moderate challenge", "Severe challenge")) %>% 
  count(Barrier, wt=proportion, name = "Sum_Mod_Sev") %>% 
  arrange(Sum_Mod_Sev)

# generate plot with flipped coordinates, reordering variables
greys <- c("#595959", 
           "#778899", 
           "#a6a6a6", 
           "#ededed")
# "#DCDCDC")


# plot `Level of Challenge` for each barrier
# challenge_percents %>%
#   ggplot(aes(x=`Level of Challenge`,
#              y=proportion, 
#              # fill=`Level of Challenge` # this conflicts with geom_text()
#              )) +
#   geom_bar(stat = "Identity") +
#   # light bars to indicate overall proportions
#   # geom_bar(data = challenge_props_df, 
#   #          aes(x=`Level of Challenge`,
#   #              y=proportion, fill = "grey", alpha = 0.3),
#   #          stat = "Identity") +
#   labs(y = "percentage of respondents", x= "")+
#   facet_wrap(vars(factor(Barrier)), ncol = 2) +
#   scale_y_continuous(labels = scales::percent)  +
#   theme_gray(base_size = 20, base_family = "sans") +
#   theme(line = element_line(colour = "black"), 
#         rect = element_rect(fill = "white", linetype = 0, colour = NA))+
#   theme(legend.position = "None") +
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
#   geom_text(data = test_results,
#             mapping = aes(label = `adj. p-val`, x = 4, y = 0.75), size = 6) +
#   coord_flip()+
#   # scale_fill_manual(values = greys, labels= unique(challenge_percents$`Level of Challenge`))+
#   theme(axis.text.y = element_text(hjust = 1.1)) +
#   theme(panel.grid.minor=element_blank())
# 
# ggsave() the last plot displayed
# ggsave("figure_02_survey2.png", 
#        units = "in", 
#        height = 15, 
#        width = 18)                               

# plot `Level of Challenge` for each barrier
challenge_percents %>%
  ggplot(aes(x=factor(Barrier, 
                      # levels = challenge_percents_ModSev$Barrier
                      levels = unique(challenge_percents_Not$Barrier)
                      ),
             y=proportion, 
             fill=`Level of Challenge` # this conflicts with geom_text()
  )) +
  geom_bar(stat = "Identity") +
  labs(y = "", x= "")+
  # facet_wrap(vars(factor(Barrier)), ncol = 2) +
  scale_x_discrete(limits = (levels(challenge_percents$Barrier))) +
  scale_y_continuous(n.breaks=6, labels = scales::percent)  +
  theme_gray(base_size = 24, base_family = "sans") +
  theme(line = element_line(colour = "black"), 
        rect = element_rect(fill = "white", linetype = 0, colour = NA))+
  theme(legend.text = element_text(size = 20),
        legend.background = element_rect(), 
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(fill=guide_legend(reverse = TRUE))+
  theme(panel.grid.major =
          element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        # unfortunately, can't mimic subtitles
        plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        strip.background = element_rect())+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
  # geom_text(data = test_results_bar,
  #           mapping = aes(label = `adj. p-val`, x = Barrier, y = 0.75), size = 6) +
  coord_flip()+
  scale_fill_manual(values = greys, labels= unique(challenge_percents$`Level of Challenge`))+
  # theme(axis.text.y = element_text(hjust = 1.1)) +
  theme(panel.grid.minor=element_blank())


# ggsave() the last plot displayed
ggsave("figure_02alt_survey2.png", 
       units = "in", 
       height = 8, 
       width = 18)                               
