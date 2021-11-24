## Script for the logit regression, between 5 year belief and partnership. 

library(tidyverse)
library(survey)
library(haven)
library(ggeffects)
library(gt)
library(plotly)

marydata <- read_stata("crrc_eu_2021.dta") 

## At the time of upload there was no link to the dataset
## It will be available at : https://caucasusbarometer.org/en/

label <- c("Break off","Legal skip","None","Interviewer error","RA","DK","Georgian Dream-Democratic Georgia",
           "United National Movement",
           "Movement for Liberty-European Georgia",
           "Alliance of Patriots of Georgia",
           "Georgian Labor Party",
           "Democratic Movement-United Georgia",
           "Aghmashenebeli Strategy",
           "Girchi",
           "Lelo",
           "Citizens",
           "Other")


NP <- c("-5","-1",300:999)

urban <- c(21,22,23,24,412,422)
rural <- c(31,32,33,34,411,421)

kaeu <- marydata %>%
  filter(PARTCLSEU %in% NP) %>%
  mutate(PARTCLSEU = case_when(
    PARTCLSEU == -9 ~ "Break Off",
    PARTCLSEU == -7 ~ "Legal Skip",
    PARTCLSEU == -3 ~ "Interviewer error",
    PARTCLSEU == -5 | PARTCLSEU == -1 ~ "DK/No Party",
    PARTCLSEU == -2 ~ "Refused to answer",
    PARTCLSEU == 301 ~ "Georgian Dream",
    TRUE ~ "Opposition"
  )) %>%
  mutate(settlement = case_when(
    substratum == 10 ~ "capital",
    substratum %in% urban ~ "other urban",
    TRUE ~ "rural"
  )) %>%
  mutate(language = case_when(
    KNOWGEO == 1 ~ "Basic to no knowledge",
    KNOWGEO == 2 ~ "Basic to no knowledge",
    KNOWGEO == 3 ~ "Intermediate to Advanced",
    KNOWGEO == 4 ~ "Intermediate to Advanced",
    TRUE ~ "99"
  )) %>%
  filter(!(language == "99"))%>%
  mutate(GEEUMEMB = case_when(
    GEEUMEMB == 1 ~ 1,
    TRUE ~ 0)) %>%
  mutate(age_group = case_when(
    17 > age | age < 35 ~ "18-34",
    34 > age | age < 55 ~ "35-54",
    TRUE ~ "55+")) %>%
  mutate(ethni = case_when(
    ETHNIC == 3 ~ "Georgian",
    TRUE ~ "Minority"
  )) %>%
  mutate(edu = case_when (
    EDUDGR >= 6 ~ "Terceriary Education",
    TRUE ~ "Basic and Middle  Education")) %>%
  mutate(employment = case_when(
    6 >= EMPLSIT& EMPLSIT >= 5 ~ "Working",
    TRUE ~ "Not working"
  )) %>%
  mutate(wealth = case_when(
    CURRUNG == 1 | CURRUNG == 2 ~ "Poor",
    CURRUNG == 3 ~ "Fair",
    CURRUNG >= 4 ~ "Good",
    TRUE ~ "NR"
  )) %>%
  filter(wealth != "NR") %>%
  mutate(PARTCLSEU = factor(PARTCLSEU),
         GEEUMEMB = factor(GEEUMEMB),
         age_group = factor(age_group),
         sex = factor(sex),
         ethni = factor(ethni),
         edu = factor(edu),
         employment = factor(employment),
         RELCOND = factor(RELCOND),
         settlement = factor(settlement)) 

kaeu_svy<-svydesign(~psu, strata = kaeu$substratum, weights = kaeu$indwt, data = kaeu)
logit_reg_membeu_gnl<-svyglm(GEEUMEMB~PARTCLSEU+
                           age_group+
                           settlement+   ## recoded from substratum
                           sex+ ## male female
                           ethni+ ## Georgian or not
                           edu+ ## bachelor+ or not
                           employment ## divided in working or not
                           , design = kaeu_svy, family = binomial)
summary(logit_reg_membeu)
party_glm <- ggemmeans(logit_reg_membeu, "PARTCLSEU")
partboth_glm <- ggemmeans(logit_reg_membeu, "ethni") %>%
  rbind(party_glm) %>%
  mutate(predicted = (predicted*100)) 


## testing 

logit_reg_memb<-svyglm(GEEUMEMB~PARTCLSEU+
                           age_group+
                           settlement+   ## recoded from substratum
                           sex+ ## male female
                           ethni+ ## Georgian or not
                           edu+ ## bachelor+ or not
                           employment+
                          language## divided in working or not
                         , design = kaeu_svy, family = binomial)


## this is the interaction
interact <- kaeu %>%
  mutate("langethni" = case_when(
    language == "Basic to no knowledge" & ethni == "Georgian" ~ "georgian*knows", ## there are only four in this category and I assumed this was wrong
    language == "Basic to no knowledge" & ethni == "Minority" ~ "minority*noknowledge",
    language == "Intermediate to Advanced" & ethni == "Georgian" ~ "georgian*knows",
    language == "Intermediate to Advanced" & ethni == "Minority" ~ "minority*knows"
  )) %>%
  mutate(langethni = factor(langethni))
##now the analysis of the model

interact_svy<-svydesign(~psu, strata = interact$substratum, weights = interact$indwt, data = interact)
logit_reg_memb<-svyglm(GEEUMEMB~PARTCLSEU+
                         age_group+
                         settlement+   ## recoded from substratum
                         sex+ ## male female
                         edu+ ## bachelor+ or not
                         employment+
                         langethni
                        , design = interact_svy, family = binomial)


##plotting the pprobabilities

for_fit <- c("Georgian","Minority")

plot_glm <- ggemmeans(logit_reg_membeu_gnl, "PARTCLSEU") %>%
  rbind(ggemmeans(logit_reg_memb, "langethni"))%>%
  mutate(predicted = (predicted*100),
         std.error = (std.error*100)) %>%
  filter(!(x %in% for_fit))


plot_interact <- ggemmeans(logit_reg_memb,"langethni") %>%
  mutate(predicted = (predicted*100),
         std.error = (std.error*100))


ggplot(plot_glm, aes(x,predicted))+   ## together it looks quite ugly, so im gonna separate the interaction
  geom_point()+
  geom_segment(aes(x=x,xend=x,y=conf.low*100,yend=conf.high*100))+
  geom_text(label = round(plot_glm$predicted),size=3,vjust=1.5)+
  coord_flip()+
  ylim(0, 50)+
  labs(
      title="Believing that Georgia will join the EU in 5 or less years",
      x = element_blank(),
      caption="
This graph was generated from a logistic regression model which included sex, 
ethnicity paired with knowledge of georgian, employment status, age group and party support",
      y= "Predicted Probabilities of one believing Georgia will join in 5 or less years (%)")+
  theme_bw()+
  scale_x_discrete(labels = c("georgian*knows" = "Georgian","minority*knows" = "Minority 
                              w/ Intermediate+ Georgian Language","minority*noknowledge" = "Minority 
                              w/ Basic to no knowledge Georgian Language"))+
  theme(plot.title = element_text(size = 14),
        plot.margin= margin(6,3,1,-50,unit = "pt"))


ggplot(plot_interact, aes(x,predicted))+  
  geom_point()+
  geom_segment(aes(x=x,xend=x,y=conf.low*100,yend=conf.high*100),size=1)+
  geom_text(label = round(plot_interact$predicted,1),size=3,vjust=1.5)+
  coord_flip()+
  ylim(0, 50)+
  labs(
    title="Believing that Georgia will join the EU in 5 or less years",
    x = element_blank(),
    caption="
This graph was generated from a logistic regression model which included sex, 
ethnicity paired with knowledge of georgian, employment status, age group and party support",
    y= "Predicted Probabilities of one believing Georgia will join in 5 or less years (%)")+
  theme_bw()+
  scale_x_discrete(labels = c("georgian*knows" = "Georgian 
                              w/ Intermediate Georgian language","minority*knows" = "Minority 
                              w/ Intermediate+ Georgian Language","minority*noknowledge" = "Minority 
                              w/ Basic to no knowledge Georgian Language"))+
 theme(plot.title = element_text(size = 14),
       plot.margin= margin(6,3,1,-50,unit = "pt"))
   