## Immigration
library(tidyverse)
library(stringi)
Imm = read.csv("C:/Users/kfur006/Desktop/Random Data/Immigration Data Explorer/R4_Residence_Occupations_by_Decision_Type_and_Nationality_and_Occupation.csv") %>% 
  mutate(#Nationality = factor(Nationality),
    Occupation = factor(Occupation, level = sort(unique(Occupation))),
    Calendar.Year = as.numeric(stri_trim(gsub("[([:alpha:]+[:punct:]+)]", "", Calendar.Year)))
  ) %>% 
  mutate(key = paste(Calendar.Year,
                     Nationality,
                     Occupation))

ImmTotalApp <- Imm %>% group_by(Calendar.Year,
                                Nationality,
                                Occupation,
                                key) %>%
  summarise(Total.App = sum(Count)) %>% 
  ungroup() %>% 
  select(c("key", "Total.App"))

ImmData <- Imm %>% left_join(., ImmTotalApp, by = "key") %>% 
  select(-key) %>% 
  mutate(Dec.Rate = Count/Total.App) %>% 
  filter(Total.App != 0) 

ImmData %>% filter(Nationality == "Japan") %>% 
  group_by(Calendar.Year,
           Nationality
           ) %>% 
  summarise(Total.App2 = sum(Count)) %>% 
  ggplot(., aes(x = Calendar.Year, y = Total.App2)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Number of Residence Application (Japan)",
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme(text = element_text(size = 15, face = "bold"))


ImmData %>% 
  group_by(Calendar.Year,
  ) %>% 
  summarise(Total.App2 = sum(Count)) %>% 
  ggplot(., aes(x = Calendar.Year, y = Total.App2)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Number of Residence Application",
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme(text = element_text(size = 15, face = "bold"))
