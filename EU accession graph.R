library(tidyverse)
library(haven)
library(ggnewscale)

## This is for the points graph showcasing the difference in time between
## each accession process, and the one Georgia aspires to. 

nobarriers <- data.frame("Georgia",2026,2024,2)
names(nobarriers) <- c("Name","Accession","Applied","Dif")

asc_data <- read_csv("Accession nego data.csv")

asc_data1 <- asc_data %>%
            filter(Applied > 0) %>%
  rbind(nobarriers)%>%
  mutate(Member = case_when(
    Name == "Georgia" ~ "No",
    TRUE ~ "Yes"
  )) %>%
  mutate(Name=factor(Name, levels=c("Georgia","Croatia","Bulgaria","Romania","Cyprus","Czech Republic",
                                    "Estonia","Hungary","Latvia","Lithuania","Malta","Poland",
                                    "Slovakia","Slovenia","Austria","Finland","Sweden","Portugal",
                                    "Spain","Greece","Denmark","Ireland")))

colors_members_line <- c("Yes" = "dodgerblue4", "No" = "red")  
colors_members_dot_app <- c("Yes" = "chartreuse1", "No" = "seagreen3") 
data4text <- asc_data1$Member=="No"

ggplot(asc_data1,aes(Name, Accession,groups=Member))+
      geom_point(size = 3, color = "yellow")+
      geom_point(aes(Name,Applied,color= "dodgerblue4"),size = 3)+
      scale_color_manual(values=colors_members_dot_app)+
      new_scale_color()+
      geom_text(data=asc_data1[asc_data1$Member=="Yes",],aes(Name, Applied, label=Applied), vjust = -0.5, size = 4)+
      geom_text(data=asc_data1[asc_data1$Member=="No",],aes(Name, Applied, label=Applied), vjust = -0.5, size = 4,hjust = 0.95)+
      geom_segment(aes(xend=Name, yend=Applied,color = Member),size = 1.5, vjust = -1)+
      scale_color_manual(values=colors_members_line)+
      geom_text(aes(Name, Accession, label=Accession), vjust = -0.5, size = 4)+
      coord_flip()+
      labs(x="Non-founding countries",
      y="Years",
      title = "Timelapse between candidate application and EU accession")+
      ylab("Applied for candidacy and accession to the EU")+
      scale_y_continuous(limits = c(1960,2030), expand = c(0,0))+
      theme_classic()+
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

