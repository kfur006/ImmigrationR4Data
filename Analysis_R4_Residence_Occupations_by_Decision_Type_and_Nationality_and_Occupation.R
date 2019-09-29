## Immigration
library(tidyverse)
library(stringi)
library(ggpubr)
library(gghighlight)
Imm = read.csv(#"C:/Users/kfur006/Desktop/Random Data/Immigration Data Explorer/R4_Residence_Occupations_by_Decision_Type_and_Nationality_and_Occupation.csv"
               #"C:/Users/key_a/Documents/GitHub/ImmigrationR4Data/R4_Residence_Occupations_by_Decision_Type_and_Nationality_and_Occupation.csv",
               "E:/Analysis/Immigration/W3_Work_Occupations_by_Decision_Type_and_Nationality_and_Occupation.csv") %>% 
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



  


TextSize = 17

ImmTheme = theme(text = element_text(size = TextSize, face = "bold"),
                 plot.title = element_text(size = TextSize, face = "bold"),
                 plot.subtitle = element_text(size = TextSize, face = "bold"))

## Approve Rate
ImmData %>% 
  #filter(Occupation == "Chef") %>% 
  group_by(Calendar.Year, Decision.Type) %>% 
  summarise(Count2 = sum(Count)) %>% 
  spread(., Decision.Type, Count2) %>% 
  mutate(Rate = Approved/(Approved+Declined)) %>% 
  ggplot(., aes(x = Calendar.Year, y = Rate*100)) +
  geom_line(size = 2) +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Approve Rate",
       x = "",
       y = "Approve Rate",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme

## Overall App Number
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
  theme_bw() +
  ImmTheme

## Overall App Number Japan
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
  theme_bw() +
  ImmTheme





ggarrange(ImmData %>% 
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
            theme_bw() +
            ImmTheme,
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
            theme_bw() +
            ImmTheme,
          ncol = 1)



## Overall App Number by Occupation (Excl. Not Recorded)
AnnX <- median(ImmData$Calendar.Year)
#AnnY <- mean(ImmData$Total.App)
ImmData %>% 
  filter(!Occupation %in% c("(not recorded)", "Not Stated")) %>% 
  group_by(Calendar.Year, Occupation
  ) %>% 
  summarise(Total.App2 = sum(Count)) %>%
  gghighlight_line(., aes(x = Calendar.Year, y = Total.App2, colour = Occupation), predicate = max(Total.App2), 
                   max_highlight = 10, size = 4) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Number of Residence Application",
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme +
  annotate("text", x = AnnX, y = 500, label = "Power In Numbers", size = 15, colour = "grey")


## Japanese App Number by Occupation (Excl. Not Recorded)
AnnX <- median(ImmData$Calendar.Year)
#AnnY <- mean(ImmData$Total.App)
ImmData %>% 
  filter(!Occupation %in% c("(not recorded)", "Not Stated"),
         Nationality == "Japan",
         Calendar.Year != 2009) %>% 
  group_by(Calendar.Year, Occupation
  ) %>% 
  summarise(Total.App2 = sum(Count)) %>%
  gghighlight_line(., aes(x = Calendar.Year, y = Total.App2, colour = Occupation), predicate = max(Total.App2), 
                   max_highlight = 15, size = 4) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Number of Application",
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme +
  annotate("text", x = AnnX, y = 500, label = "Power In Numbers", size = 15, colour = "grey")


## Overall App Number Early Childhood
ImmData %>%
  filter(str_detect(Occupation, "Early")) %>% 
  group_by(Calendar.Year, Occupation) %>% 
  summarise(Total.App2 = sum(Count)) %>%
  ggplot(., aes(x = Calendar.Year, y = Total.App2)) +
  geom_line(size = 2) +
  ylim(0, 200) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Number of Residence Application Early Childhood",
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme



## Approve Rate Early Childhood
ImmData %>% 
  filter(str_detect(Occupation, "Early")) %>% 
  group_by(Calendar.Year, Decision.Type) %>% 
  summarise(Count2 = sum(Count)) %>% 
  spread(., Decision.Type, Count2) %>% 
  mutate(Rate = Approved/(Approved+Declined)) %>% 
  ggplot(., aes(x = Calendar.Year, y = Rate*100)) +
  geom_line(size = 2) +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = "Approve Rate of Early Childhood",
       x = "",
       y = "Approve Rate",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme +
  geom_hline(yintercept  = ImmData %>% 
               #filter(Occupation == "Chef") %>% 
               group_by(Calendar.Year, Decision.Type) %>% 
               summarise(Count2 = sum(Count)) %>% 
               spread(., Decision.Type, Count2) %>% 
               mutate(Rate = Approved/(Approved+Declined)) %>%
               select(-c("Approved", "Declined")) %>%
               ungroup() %>% 
               summarise(RateAve = mean(Rate)) %>% 
               as.numeric()*100,
             colour = "red",
             size = 2,
             linetype = "dashed")





## Overall App Number Early Childhood
Occ <- 'Hotel|Motel'
Occup <- as.character(ImmData$Occupation[which(str_detect(unique(ImmData$Occupation), Occ))][1])

ImmData %>%
  filter(str_detect(Occupation, Occup)) %>% 
  group_by(Calendar.Year, Occupation) %>% 
  summarise(Total.App2 = sum(Count)) %>%
  ggplot(., aes(x = Calendar.Year, y = Total.App2)) +
  geom_line(size = 2) +
  ylim(0, 200) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = paste("Number of Residence Application of", Occup),
       x = "",
       y = "Number of Applications",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme



## Approve Rate

ImmData %>% 
  filter(str_detect(Occupation, Occup)) %>% 
  group_by(Calendar.Year, Decision.Type) %>% 
  summarise(Count2 = sum(Count)) %>% 
  spread(., Decision.Type, Count2) %>% 
  mutate(Rate = Approved/(Approved+Declined)) %>% 
  ggplot(., aes(x = Calendar.Year, y = Rate*100)) +
  geom_line(size = 2) +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(min(ImmData$Calendar.Year), max(ImmData$Calendar.Year), by = 1)) +
  labs(title = paste("Approve Rate of",  Occup),
       x = "",
       y = "Approve Rate",
       caption = "Data: Immigration NZ") +
  theme_bw() +
  ImmTheme +
  geom_hline(yintercept  = ImmData %>% 
               #filter(Occupation == "Chef") %>% 
               group_by(Calendar.Year, Decision.Type) %>% 
               summarise(Count2 = sum(Count)) %>% 
               spread(., Decision.Type, Count2) %>% 
               mutate(Rate = Approved/(Approved+Declined)) %>%
               select(-c("Approved", "Declined")) %>%
               ungroup() %>% 
               summarise(RateAve = mean(Rate)) %>% 
               as.numeric()*100,
             colour = "red",
             size = 2,
             linetype = "dashed")


## Work
load("E:/Analysis/Immigration/rda/W3_work_occupations.rda")




## Work Visa Analysis
library(tidyverse)
library(stringi)
library(ggpubr)
library(gghighlight)
load("E:/Analysis/Immigration/rda/W3_work_occupations.rda")

head(W3_work_occupations)
str(W3_work_occupations)

W3_work_occupations %>% 
  filter(Nationality == "Argentina",
         Date == "2008-07-31",
         Occupation == "Chef",
         `Application Substream` == "Skilled Work")

W3_work_occupations %>% 
  filter(`Application Substream` == "Skilled Work") %>% 
  select("Date", 
         "Decision Type",
         "Nationality",
         "Occupation Skill Level",
         "Occupation",
         #"Region",
         "Count") %>% 
  group_by(Date, 
           `Decision Type`,
           Nationality,
           `Occupation Skill Level`,
           Occupation,
           #Region,
           Count) %>% 
  summarise(n = sum(Count)) %>% 
  filter(n != 0,
         Nationality == "Japan") %>% 
  #ggplot(data = ., aes(x = Date, y = Count, colour = Occupation)) +
  gghighlight_line(., aes(x = Date, y = n, colour = Occupation), predicate = max(n), 
                   max_highlight = 10, size = 2)




test <- W3_work_occupations %>% 
  filter(`Application Substream` == "Skilled Work",
         Nationality == "Japan") %>% 
  select("Date", 
         #"Decision Type",
         "Nationality",
         #"Occupation Skill Level",
         "Occupation",
         #"Region",
         "Count") %>% 
  group_by(Date, 
           #`Decision Type`,
           Nationality,
           #`Occupation Skill Level`,
           Occupation,
           #Region
  ) %>% 
  summarise(n = sum(Count))

test %>% 
  filter(Nationality == "Japan") %>% 
  gghighlight_line(., aes(x = Date, y = n, colour = Occupation), predicate = max(n), 
                   max_highlight = 10, size = 2)


