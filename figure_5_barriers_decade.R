# Create "gender frequency of barriers" figure for Survey 2
# modeled on Figure 4 of Williams et al. (2019) using figure_04.R

# prerequisites
rm(list = ls())
require(tidyverse)
require(ggthemes)
require(ggpubr)
require(infer)

# start with the `Merged_Data` set produced in Chapter 0 and rename columns
Merged_Data <- read_csv("Merged_Data_Anonymous.csv")
# set decade of degree
Merged_Data <- Merged_Data %>% 
  rename(`Decade of Degree` = `Q14 In which year did you earn your highest academic degree?`) %>% 
  mutate(`Decade of Degree` = case_when(`Decade of Degree` == "2010-2019" ~ "2010s",
                                        `Decade of Degree` == "2000-2009" ~ "2000s",
                                        `Decade of Degree` == "1990-1999" ~ "1990s",
                                        `Decade of Degree` == "1990-1999" ~ "1990s",
                                        `Decade of Degree` == "1980-1989" ~ "1980s",
                                        is.na(`Decade of Degree`) ~ "Unknown Decade",
                                        TRUE ~ `Decade of Degree`))

# set level of training
Merged_Data <- Merged_Data %>% 
  rename(`Bioinformatics Training` = `Q12 Which of the following best describes your level of bioinformatics training? Select ALL that apply.`) %>%
  mutate(TrainingGroups = case_when(str_detect(`Bioinformatics Training`, "graduate") ~ "At Least Some Coursework",
                                    str_detect(`Bioinformatics Training`, "workshops") ~ "At Least Workshops/Bootcamps",
                                    str_detect(`Bioinformatics Training`, "self") ~ "Self-taught Only",
                                    str_detect(`Bioinformatics Training`, "no training/experience") ~ "No Training",
                                    is.na(`Bioinformatics Training`) ~ "Unknown Training",
                                    TRUE ~ "Unknown Training"),
  )

# Find decades with <30 participants (and unknown)
Decade_count <- Merged_Data %>% 
  count(`Decade of Degree`)
Decade_keep <- Decade_count %>% 
  filter(n >30) %>% 
  pull(`Decade of Degree`)

# count `Decade of Degree` for each Training Group
DegreeDecade_Training_count <-  Merged_Data %>% 
  select(`Decade of Degree`, TrainingGroups) %>% 
  mutate(TrainingGroups = factor(TrainingGroups, 
                                 levels = c("At Least Some Coursework", "At Least Workshops/Bootcamps",
                                            "Self-taught Only", "No Training"))
         # levels = c("No Training", "Self-taught Only",
         #            "At Least Workshops/Bootcamps",
         #            "At Least Some Coursework"))
         ) %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>%
  drop_na() %>% 
  group_by(`Decade of Degree`) %>% 
  count(TrainingGroups, name = "count") %>% 
  mutate(proportion = count / sum(count))

# conduct test for association between degree decade and level of bioinformatics training
# hypothesis test: chi-squared 
Merged_Data %>% 
  select(`Decade of Degree`, TrainingGroups) %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>% 
  drop_na() %>% 
  chisq_test(TrainingGroups ~ `Decade of Degree`)

# plot frequency of level of bioinformatics training by decade of degree
greys <- c("#595959", 
           "#778899", 
           "#a6a6a6", 
           "#ededed")
# "#DCDCDC")

# fig5a <- DegreeDecade_Training_count %>%
#   ggplot(aes(x=`Decade of Degree`,
#              # factor(`Decade of Degree`, levels = "1980s\n(n=56)", "1990s\n(n=120)",
#              #        "2000s\n(n=189)", "2010s\n(n=101)"),
#              fill = TrainingGroups, 
#              y=proportion)) +
#   geom_bar(position = "dodge", stat = "Identity")+
#   labs(y = "Percentage of respondents", x= "Decade of terminal degree",
#        fill = "Bioinformatics training") +
#   scale_y_continuous(labels = scales::percent)  +
#   theme_gray(base_size = 20, base_family = "sans") +
#   theme(line = element_line(colour = "black"), 
#         rect = element_rect(fill = "white", linetype = 0, colour = NA))+
#   theme(legend.background = element_rect(), 
#         legend.position = "right",
#         # legend.title = element_blank()
#   ) +
#   theme(panel.grid.major =
#           element_line(colour = "grey"),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect())+
#   theme(axis.title.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   theme(strip.text.x = element_text(size = 18, face = "bold"))+
#   theme(plot.background = element_rect(fill = "white"))+
#   theme(panel.background = element_rect(fill = "white"))+
#   theme(panel.grid.major.y = element_blank())+
#   theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
#   coord_flip()+
#   scale_fill_manual(values = greys, labels= unique(DegreeDecade_Training_count$TrainingGroups))+
#   guides(fill=guide_legend(reverse = TRUE))+
#   theme(axis.text = element_text(size = 18)) +
#   theme(panel.grid.minor=element_blank())

fig5alt <-
  DegreeDecade_Training_count %>%
  ggplot(aes(x=`Decade of Degree`,
             # factor(`Decade of Degree`, levels = "1980s\n(n=56)", "1990s\n(n=120)",
             #        "2000s\n(n=189)", "2010s\n(n=101)"),
             fill = TrainingGroups, 
             y=proportion)) +
  geom_bar(# position = "dodge", 
           stat = "Identity")+
  labs(y = "Percentage of respondents", x= "Decade of terminal degree",
       fill = "Bioinformatics training") +
  scale_y_continuous(n.breaks=6, labels = scales::percent)  +
  theme_gray(base_size = 20, base_family = "sans") +
  theme(line = element_line(colour = "black"), 
        rect = element_rect(fill = "white", linetype = 0, colour = NA))+
  theme(legend.background = element_rect(), 
        legend.position = "right",
        # legend.title = element_blank()
  ) +
  theme(panel.grid.major =
          element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect())+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
  coord_flip()+
  scale_fill_manual(values = greys, 
                    labels= unique(DegreeDecade_Training_count$TrainingGroups))+
  guides(fill=guide_legend(reverse = TRUE))+
  theme(axis.text = element_text(size = 18)) +
  theme(panel.grid.minor=element_blank())


fig5null <- Decade_count %>% 
  ggplot(aes(`Decade of Degree`,n))+geom_blank()+theme_void()

# ggarrange(fig5a, fig5null,
#           ncol = 2, nrow = 1, labels = list("A",""), font.label = list(size = 18), 
#           widths = c(3,1), hjust = 0)
# 
# # ggsave() the last plot displayed
# ggsave("figure_05a_survey2.png", 
#        units = "in", 
#        height = 4, 
#        width = 14)                               

ggarrange(fig5alt, fig5null,
          ncol = 2, nrow = 1, labels = list("A",""), font.label = list(size = 18), 
          widths = c(97,3), hjust = 0)

# ggsave() the last plot displayed
ggsave("figure_05alt_survey2.png", 
       units = "in", 
       height = 4, 
       width = 14)                               

# Select `Decade of Degree` & "I lack" level of challenge columns, replace names & make data tidy
Challenge_df <- Merged_Data %>% 
  select(respID, 
         `Decade of Degree`, 
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
  pivot_longer(-c(respID, `Decade of Degree`), names_to = "Barrier", values_to = "Level of Challenge") %>% 
  mutate(`Level of Challenge` = replace_na(`Level of Challenge`, "Not a challenge")) %>% 
  mutate(`Level of Challenge` = factor(`Level of Challenge`, 
                                       levels = c("Not a challenge", "Minor challenge", 
                                                  "Moderate challenge", "Severe challenge")))



# conduct test for association between degree decade and each barrier
test_results <- unique(Challenge_df$Barrier) %>%
  map_df(function(x) { 
    Challenge_df %>% 
      filter(`Decade of Degree` %in% Decade_keep) %>% 
      filter(Barrier == x) %>% 
      chisq_test(`Level of Challenge` ~ `Decade of Degree`)
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

# Conduct test for association between degree decade and no challenges for any barrier
# Add column to identify participants with no challenges
AnyChallenge_df <- Challenge_df %>% 
  pivot_wider(names_from = Barrier, values_from = `Level of Challenge`) %>% 
  mutate(NoChallenges = if_all(.cols = `I lack expertise in bioinformatics`:`My students lack interest`,
                                .fns = ~ . == "Not a challenge"))

# count "No challenges" for each decade
NoChallenge_Decade_count <-  AnyChallenge_df %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>% 
  group_by(`Decade of Degree`) %>% 
  count(NoChallenges, name = "count") %>% 
  mutate(proportion = count / sum(count))

# hypothesis test: chi-squared after removing decades with few responses
AnyChallenge_df %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>% 
  chisq_test(NoChallenges ~ `Decade of Degree`)

# plot with !NoChallenges frequency by decade
fig5b <- NoChallenge_Decade_count %>%
  filter(NoChallenges == FALSE) %>% 
  ggplot(aes(x=`Decade of Degree`, 
             y=proportion))+
  geom_bar(stat = "Identity")+
  labs(y = "Percentage facing challenges", x= "") +
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
        strip.background = element_rect())+
  theme(#axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
  coord_flip()+
  theme(axis.text = element_text(size = 18)) +
  theme(panel.grid.minor=element_blank())

# Select `Decade of Degree` & "I lack" level of challenge columns, replace names & make data tidy
teaching_df <- Merged_Data %>% 
  select(`Decade of Degree`, 
         Q3TeachBioinfor) %>% 
  mutate(DontTeachBioinfor = str_detect(Q3TeachBioinfor, "not"))

# Conduct test for association between degree decade and not teaching bioinformatics
# count "Don't teach bioinformatics" for each decade
NotTeachBioinfo_Decade_count <-  teaching_df %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>% 
  group_by(`Decade of Degree`) %>% 
  count(DontTeachBioinfor, name = "count") %>% 
  mutate(proportion = count / sum(count))

# hypothesis test: chi-squared after removing decades with few responses
teaching_df %>% 
  filter(`Decade of Degree` %in% Decade_keep) %>% 
  drop_na() %>% 
  chisq_test(DontTeachBioinfor ~ `Decade of Degree`)

# plot TeachBioinfor frequency by decade
fig5c <- NotTeachBioinfo_Decade_count %>%
  filter(DontTeachBioinfor == FALSE) %>% 
  ggplot(aes(x=`Decade of Degree`, 
             y=proportion))+
  geom_bar(stat = "Identity")+
  labs(y = "Percentage integrating bioinformatics", x= "") +
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
        strip.background = element_rect())+
  theme(#axis.title.x=element_blank(),
    axis.ticks.x=element_blank()) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major.y = element_blank())+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5))+
  coord_flip()+
  theme(axis.text = element_text(size = 18)) +
  theme(panel.grid.minor=element_blank())

ggarrange(fig5b, fig5c + rremove("y.text"), 
          ncol = 2, nrow = 1, labels = list("B", "C"), font.label = list(size = 18))

# ggsave() the last plot displayed
ggsave("figure_05bc_survey2.png", 
       units = "in", 
       height = 4, 
       width = 14)                               



