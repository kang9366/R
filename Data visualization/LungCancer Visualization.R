library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(scales)

cancer <- read.csv("./data/lung-cancer.csv")
cancer$Age
cancer$LC %<>% as.factor(.)

#1
cancer_data <- table(cancer$LC) %>% 
  as.data.frame()
names(cancer_data)[1] <- "type"
names(cancer_data)[2] <- "num"

ggplot(cancer_data, aes(x=type, y=num, fill=type)) + 
  geom_bar(stat="identity", width=0.5) + 
  theme_wsj() + 
  scale_colour_wsj("colors6") +
  theme(legend.position = "none") +
  geom_text(aes(y=num, label = num), colour="black", size=5, vjust=1.5)
  
#2


#3
male_cancer <- cancer %>% 
  filter(LC=="LungCancer" & Sex == "Male") %>% 
  nrow()
female_cancer <- cancer %>% 
  filter(LC == "LungCancer", Sex == "Female") %>% 
  nrow()
male_noCancer <-cancer %>% 
  filter(LC == "NoCancer" & Sex == "Male") %>% 
  nrow()
female_noCancer <- cancer %>% 
  filter(LC == "NoCancer" & Sex == "Female") %>% 
  nrow()
prop <- c(male_cancer/(male_cancer+female_cancer),
         female_cancer/(male_cancer+female_cancer),
         male_noCancer/(male_noCancer+female_noCancer),
         female_noCancer/(male_noCancer+female_noCancer)) %>% 
  round(., digit=2)
gender_data <- data.frame(gender = rep(c("male", "female"), times=2) %>% factor(),
                          num = c(male_cancer, female_cancer, male_noCancer, female_noCancer),
                          label = rep(c("LungCancer", "NoCancer"), each=2) %>% factor(),
                          prop = prop,
                          cum = c(prop[1], prop[1]+prop[2], prop[3], prop[3]+prop[4]))

ggplot(gender_data, aes(x=label, y=num, fill=gender)) + 
  geom_bar(stat="identity", position="stack", width=0.6) +
  geom_text(aes(y=num, label=num), colour = "black", size=6, vjust=1.5) +
  theme_wsj()

ggplot(gender_data, aes(x=label, y=prop, fill=gender)) + 
  geom_bar(stat="identity", position="stack", width=0.6) +
  geom_text(aes(y=cum, label=paste0(prop*100, "%")), colour = "black", size=5, vjust=1.5) +
  scale_y_continuous(labels=percent)+
  theme_wsj()

#4
period_cancer <- cancer %>% 
  filter(LC == "LungCancer") %>% 
  select(Age) #%>% 
  #table() %>%
  #as.data.frame()

period_no_cancer <- cancer %>% 
  filter(LC == "NoCancer") %>% 
  select(Age) #%>% 
  #table() %>%
  #as.data.frame()

names(period_cancer)[1] <- "age"
names(period_no_cancer)[1] <- "age"
names(period_cancer)[2] <- "num"
names(period_no_cancer)[2] <- "num"

ggplot() + 
  geom_density(data=period_no_cancer, aes(x=age), fill="#BAD252", alpha=0.5) +
  geom_density(data=period_cancer, aes(x=age), fill="#E0B0BC", alpha=0.5)

ggplot(period_cancer, aes(x=age)) + 
  geom_density(fill="#E0B0BC", alpha=0.5) +
  theme_wsj()

ggplot(period_no_cancer, aes(x=age)) + 
  geom_density(fill="#E0B0BC", alpha=0.5) +
  theme_wsj()


#ggplot(period_cancer, aes(x=age, y=num)) + 
#  geom_line(aes(group=1)) + 
#  geom_point()
