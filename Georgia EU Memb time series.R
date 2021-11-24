library(tidyverse)
library(haven)
library(foreign)
library(lubridate)

## When will Georgia join the EU? -  Time series

##2021

marydata <- read_stata("crrc_eu_2021.dta")

weight_total <- sum(marydata$indwt)

cc <- c("In 5 years" ,"In 6 to 10 years","In more than 10 years","Never","Don't know")

GEEUMEMB_2021 <- marydata %>%
  select (GEEUMEMB,sex,age,Edu_level = EDUDGR,Weight = indwt) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2021)

## 2009

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2009_Only_responses_05102012.dta")


weight_total <- sum(marydata$INDWT)

GEEUMEMB_2009 <- marydata %>%
  select (GEEUMEMB,age = RESPAGE,Edu_level = EDUDGR,Weight = INDWT) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2009)

GEEUMEMB_2009

## 2011

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2011_Only_responses_05102012.dta")


weight_total <- sum(marydata$INDWT)

GEEUMEMB_2011 <- marydata %>%
  select (GEEUMEMB,RESPSEX,age = RESPAGE,Edu_level = EDUDGR,Weight = INDWT) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2011)

GEEUMEMB_2011

## 2013

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2013_Only_responses_06082013.dta")


weight_total <- sum(marydata$indwt)

GEEUMEMB_2013 <- marydata %>%
  select (GEEUMEMB = q70,sex,age,Edu_level = q86,Weight = indwt) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2013)

GEEUMEMB_2013

## 2015

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2015_May_27.08.15.dta")

weight_total <- sum(marydata$indwt)


GEEUMEMB_2015 <- marydata %>%
  select (GEEUMEMB = q68,sex,age ,Edu_level = q86,Weight = indwt) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2015)

GEEUMEMB_2015

## 2017

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2017_May_05.10.17.dta")

weight_total <- sum(marydata$indwt)

GEEUMEMB_2017 <- marydata %>%
  select (GEEUMEMB,sex,age,Edu_level = EDUDGR,Weight = indwt) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2017)

## 2019

marydata <- read_stata("https://caucasusbarometer.org/downloads/EU_2019_09.04.19_public.dta")

cc <- c("In 5 years" ,"In 6 to 10 years","In more than 10 years","Never","Don't know")

weight_total <- sum(marydata$indwt)

GEEUMEMB_2019 <- marydata %>%
  select (GEEUMEMB,sex,age,Edu_level = EDUDGR,Weight = indwt) %>%
  mutate(AGEGROUP = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-55",
    TRUE ~ "56+")) %>%
  mutate(ANSWER = case_when(
    GEEUMEMB == 1 ~ "In 5 years",
    GEEUMEMB == 2 ~ "In 6 to 10 years",
    GEEUMEMB == 3 ~ "In more than 10 years",
    GEEUMEMB == -7 ~ "Legal Skip",
    GEEUMEMB == -1 ~ "Don't know",
    GEEUMEMB == -2 ~ "Refuse to Answer",
    TRUE ~ "Never")) %>%
  group_by(ANSWER,AGEGROUP) %>%
  summarise(wfreq = sum(Weight))%>%
  mutate(wtotal_category = sum(wfreq),
         wpercentage_total = round(wfreq/weight_total*100,2)) %>%
  select(ANSWER, AGEGROUP, wpercentage_total) %>%
  filter(ANSWER %in% cc) %>%
  add_column(Year = 2019)

## Mergeing the tables

GEEUMEMB_timeseries <- GEEUMEMB_2009 %>%
  full_join(GEEUMEMB_2011) %>%
  full_join(GEEUMEMB_2013) %>%
  full_join(GEEUMEMB_2015) %>%
  full_join(GEEUMEMB_2017) %>%
  full_join(GEEUMEMB_2019) %>%
  full_join(GEEUMEMB_2021) %>%
  rename(Answer = ANSWER, Age = AGEGROUP, Percentage = wpercentage_total) %>%
  mutate(Year = as_date(paste(Year,"01-01",sep = "")))%>%
  group_by(Answer,Year) %>%
  summarise(Percentage_total = sum(Percentage))


## plotting line

ggplot(GEEUMEMB_timeseries)+
  geom_line(aes(Year,Percentage_total, color = Answer, linetype = Answer), size = 1.3)+
  geom_text(aes(Year,Percentage_total, label=round(Percentage_total)),
            size= 3,
            nudge_y = 1,
            check_overlap = T)+
    labs(x = "Year",
       y = "Percentage",
       title = "When Will Georgia join the EU?")+
  scale_x_date(date_breaks = "2 year", 
               date_labels = "%Y",
               limits = c(as.Date("2008/1/1"), NA))

ggsave("geo_belief_timeseries.png")

