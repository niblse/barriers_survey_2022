# Create demographics figure for Survey 2
# modeled on Figure 1 of Williams et al. (2019)
# see figure_1_demographics.R

# prerequisites
rm(list = ls())
require(tidyverse)
require(ggthemes)

# 1. create demographics table with each demographic variable, category and percentage

# Create dataframe of each desired demographic

# start with the `Merged_Data` set produced in Chapter 0 and rename columns
# Merged_Data <- read_csv("Merged_Data.csv")
Merged_Data <- read_csv("Merged_Data_Anonymous.csv")
Merged_Data <- Merged_Data %>% 
  rename(`Gender Identity` = Gender, 
         `Race/Ethnicity` = Ethnicity, 
         `Terminal Degree` = `Q33 Which of the following represents your highest academic degree?`,
         `Decade of Degree` = `Q14 In which year did you earn your highest academic degree?`,
         `Bioinformatics Training` = `Q12 Which of the following best describes your level of bioinformatics training? Select ALL that apply.`,
         `Carnegie Classification` = BASIC2018_bins_text.Current,
         `Institution Size` = SIZESET2018_bins_text.Current)


Merged_Data <- Merged_Data %>%
  mutate(MSI_status = 2*MSI.Current + HBCU.Current + HSI.Current,
            # create variable to encode four categories of MSI status:
            # 2*MSI, no (0); HBCU, no (2) ; HSI, no (0) (453 responses) = 2 (None)
            # 2*MSI, yes (1); HBCU, yes (1); HSI, no (0)  (18 responses) = 3 (HBCU)
            # 2*MSI, yes (1); HBCU, no (2) ; HSI, no (0) (27 responses) = 4 (Other MSI)
            # 2*MSI, yes (1); HBCU, no (2); HSI, yes (1) (55 responses) = 5 (HSI)
         EthnoGroups = case_when(str_detect(`Race/Ethnicity`, "European") ~ "European", 
                                 str_detect(`Race/Ethnicity`, "Scandinavian") ~ "European",
                                 str_detect(`Race/Ethnicity`, "Asian") ~ "Asian",
                                 TRUE ~ `Race/Ethnicity`),
         TrainingGroups = case_when(str_detect(`Bioinformatics Training`, "graduate") ~ "At Least Some Coursework",
                                     str_detect(`Bioinformatics Training`, "workshops") ~ "At Least Workshops/Bootcamps",
                                     str_detect(`Bioinformatics Training`, "self") ~ "Self-taught Only",
                                     str_detect(`Bioinformatics Training`, "no training/experience") ~ "No Training",
                                     is.na(`Bioinformatics Training`) ~ "Unknown Training",
                                     TRUE ~ "Unknown Training"),
         `Gender Identity` = case_when(`Gender Identity` == "F" ~ "Woman",
                                       `Gender Identity` == "M" ~ "Man",
                                       `Gender Identity` == "U" ~ "Other Gender"),
         `Terminal Degree` = case_when(
           `Terminal Degree` == "Doctoral degree or equivalent (e.g;, PhD, EdD, etc)" ~ "Doctoral",
           `Terminal Degree` == "Masters degree or equivalent (e.g., MS, MBA, MA, etc)" ~ "Masters",
           `Terminal Degree` == "Professional doctoral degrees or equivalent (MD, DVM, etc)" ~ "Professional",
           `Terminal Degree` == "Undergraduate bachelor degree or equivalent (e.g., BS, BSc, BA, etc)" ~ "Bachelor's",
           is.na(`Terminal Degree`) ~ "Unknown Degree",
           TRUE ~ "Other Degree"),
         `Decade of Degree` = case_when(`Decade of Degree` == "2010-2019" ~ "2010s",
                                        `Decade of Degree` == "2000-2009" ~ "2000s",
                                        `Decade of Degree` == "1990-1999" ~ "1990s",
                                        `Decade of Degree` == "1990-1999" ~ "1990s",
                                        `Decade of Degree` == "1980-1989" ~ "1980s",
                                        is.na(`Decade of Degree`) ~ "Unknown Decade",
                                        TRUE ~ `Decade of Degree`), 
         `MSI Designation` = case_when(MSI_status == 2 ~ "Non-Minority-Serving",
                                       MSI_status == 3 ~ "HBCU",
                                       MSI_status == 4 ~ "Other MSI",
                                       MSI_status == 5 ~ "HSI"),
         `Carnegie Classification` = case_when(
           `Carnegie Classification` == "Doctoral/Professional Universities" ~ "Doctoral/Professional",
           `Carnegie Classification` == "Master's Colleges & Universities" ~ "Master's",
           `Carnegie Classification` == "Baccalaureate Colleges" ~ "Baccalaureate",
           `Carnegie Classification` == "Associate's Colleges" ~ "Associate's",
           `Carnegie Classification` == "Baccalaureate/Associate's Colleges: Associate's Dominant" ~ "Baccalaureate/Associate's",
           `Carnegie Classification` == "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's" ~ "Baccalaureate/Associate's",
           `Carnegie Classification` == "Baccalaureate/Associate's Colleges: Associate's Dominant" ~ "Baccalaureate/Associates",
           TRUE ~ "Other Classification")
         )

# make table with demographic variables only (individual & institutional info)
demographics_subset <- Merged_Data %>% 
  select(`Gender Identity`, `Race/Ethnicity` = EthnoGroups, `Terminal Degree`, `Decade of Degree`, 
         `Bioinformatics Training` = TrainingGroups,
         `MSI Designation`, `Carnegie Classification`, `Institution Size`)

# count/percent table for each variable/category
demographics_tidy <- demographics_subset %>% 
  pivot_longer(cols = everything(),
               names_to = "Demographic_variable",
               values_to = "Category")

demographics_percents <- demographics_tidy %>% 
  group_by(Demographic_variable) %>% 
  count(Category, name = "count") %>% 
  mutate(percent = round(count/sum(count)*100)) %>% 
  arrange(Demographic_variable, desc(percent)) %>% 
  filter(Category != "Exclusively graduate/professional")

# set reverse of desired order of categories (so OK after coord_flip)
reverse_order <- c("Woman", "Man", "Other Gender", 
                   "European", "Hispanic", "African American", "Asian", "Mediterranean", "Jewish", "Middle Eastern", "Native American",
                   "Doctoral", "Professional", "Masters", "Bachelor's", "Other Degree", "Unknown Degree",
                   "2020 or later", "2010s", "2000s", "1990s", "1980s", "Before 1980", "Unknown Decade",
                   "At Least Some Coursework", "At Least Workshops/Bootcamps", "Self-taught Only", "No Training", "Unknown Training",
                   "Non-Minority-Serving", "HSI", "HBCU", "Other MSI",
                   "Doctoral/Professional", "Master's", "Baccalaureate", "Baccalaureate/Associate's", "Associate's", "Other Classification",
                   "Large or Very Large", "Medium", "Small or Very Small") %>% rev()

# put categories in desired orders
demographics_percents <- demographics_percents %>% 
  mutate(Category = factor(Category, levels = reverse_order))

# generate plot with flipped coordinates, reordering variables
demographics_percents %>%
  ggplot(aes(Category, percent))+
  geom_bar(stat = "Identity")+
  coord_flip()+
  facet_wrap(vars(factor(Demographic_variable, levels = c("Gender Identity", "Bioinformatics Training", 
                                                          "Race/Ethnicity", "MSI Designation",
                                                          "Terminal Degree",  "Carnegie Classification",
                                                          "Decade of Degree", "Institution Size"))), # to order variables
             scales = "free_y", ncol = 2) +
  theme_fivethirtyeight(base_size = 22, base_family = "sans")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.background = element_rect(fill = "white"))+
  theme(legend.background = element_rect(fill = "white"))+
  theme(strip.text.y = element_blank())+
  theme(axis.text.x = element_blank())+
  geom_text(aes(label = paste0(percent, "%"), y = percent),
            vjust =0, nudge_y = 5, nudge_x = -0.1, size = 6, color = "black") +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme( axis.line = element_line(colour = "black", 
                                  linewidth = 0.5, linetype = "solid"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(axis.line.x = element_blank())+
  theme(axis.text.y = element_text(hjust = 1))

# ggsave() saves the last plot displayed
ggsave("figure_01_survey2.png", 
       units = "in", 
       height = 15, 
       width = 18)                               

# breakdown of CURE/SURE
Merged_Data %>% 
  select(Q24CURE_SURE) %>% 
  separate(col = Q24CURE_SURE, into = c("a", "b", "c", "d"), sep = ",") %>% 
  pivot_longer(cols = everything(), names_to = "placement", values_to = "response") %>% 
  count(response)

