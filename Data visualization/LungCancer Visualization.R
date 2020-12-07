if(!require(dplyr)){install.packages("dplyr")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(ggthemes)){install.packages("ggthemes")}
if(!require(ggmosaic)){install.packages("ggmosaic")}
if(!require(scales)){install.packages("dplyr")}
if(!require(plotly)){install.packages("plotly")}

library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(scales)
library(plotly)
library(ggmosaic)

cancer <- read.csv("./data/lung-cancer.csv")
View(cancer)

str(cancer)

cancer$LC %<>% as.factor(.)
cancer$Sex %<>% as.factor(.)

###1
cancer_data <- table(cancer$LC) %>% 
  as.data.frame()
names(cancer_data)[1] <- "type"
names(cancer_data)[2] <- "num"

ggplot(cancer_data, aes(x=type, y=num, fill=type)) + 
  geom_bar(stat="identity", width=0.5) + 
  theme_wsj() + 
  scale_colour_wsj("colors6") +
  theme(legend.position = "none") +
  geom_text(aes(y=num, label = num), colour="black", size=5.5, vjust=0.5)
  
###2
period_cancer <- cancer %>% 
  select(LC, Smoking)

g <- ggplot(data=period_cancer, aes(x=LC, y=Smoking, fill=LC)) + 
  geom_violin(alpha=0.6) +
  theme(legend.position = "none") +
  geom_jitter(aes(color=LC), shape=16, position=position_jitter(0.2)) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x="", y="Smoking Period") +
  theme_solarized()

ggplotly(g)

###3
male_cancer <- cancer %>% 
  filter(LC=="LungCancer" & Sex == "Male") %>% 
  nrow()
female_cancer <- cancer %>% 
  filter(LC == "LungCancer", Sex == "Female") %>% 
  nrow()
male_no_cancer <-cancer %>% 
  filter(LC == "NoCancer" & Sex == "Male") %>% 
  nrow()
female_no_cancer <- cancer %>% 
  filter(LC == "NoCancer" & Sex == "Female") %>% 
  nrow()
prop <- c(male_cancer/(male_cancer+female_cancer),
         female_cancer/(male_cancer+female_cancer),
         male_no_cancer/(male_no_cancer+female_no_cancer),
         female_no_cancer/(male_no_cancer+female_no_cancer)) %>% 
  round(., digit=2)

gender_data <- data.frame(gender = rep(c("male", "female"), times=2) %>% factor(),
                          num = c(male_cancer, female_cancer, male_no_cancer, female_no_cancer),
                          cum_num = c(male_cancer, male_cancer+female_cancer, male_no_cancer, male_no_cancer+female_no_cancer),
                          prop = prop,
                          cum_prop = c(cumsum(prop[c(1,2)]), cumsum(prop[c(3,4)])),
                          LC = rep(c("LungCancer", "NoCancer"), each=2) %>% factor())

##stacked barplot
#num
ggplot(gender_data, aes(x=LC, y=num, fill=gender)) + 
  geom_bar(stat="identity", position="stack", width=0.6) +
  geom_text(aes(y=cum_num, label=num), colour = "black", size=6, vjust=1.5) +
  theme_wsj()

#proportion
ggplot(gender_data, aes(x=LC, y=prop, fill=gender)) + 
  geom_bar(stat="identity", position="stack", width=0.6) +
  geom_text(aes(y=cum_prop, label=paste0(prop*100, "%")), colour = "black", size=5, vjust=1.5) +
  scale_y_continuous(labels=percent)+
  theme_wsj()

#mosaic plot
g <- ggplot(gender_data) +
  geom_mosaic(aes(weight=num, x=product(gender, LC), fill=LC)) +
  theme_solarized() +
  labs(x="", y="", fill="") + 
  geom_text(data = layer_data(g1, 1) %>% 
     select(xmin:ymax) %>% 
     mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
     select(m.x, m.y)  %>% 
     mutate(string = c(12, 37, 24, 74)),
     aes(x = m.x, y = m.y, label = string),
     size = 6)
g
ggplotly(g)
###4
age_cancer <- cancer %>% 
  filter(LC == "LungCancer") %>% 
  select(Age) %>% 
  table() %>%
  as.data.frame()

names(age_cancer)[1] <- "age"
names(age_cancer)[2] <- "freq"

age_no_cancer <- cancer %>% 
  filter(LC == "NoCancer") %>% 
  select(Age) %>% 
  table() %>%
  as.data.frame()

names(age_no_cancer)[1] <- "age"
names(age_no_cancer)[2] <- "freq"

#density plot
ggplot() + 
  geom_density(data=age_no_cancer, aes(x=age), fill="#BAD252", alpha=0.5) +
  geom_density(data=age_cancer, aes(x=age), fill="#E0B0BC", alpha=0.5)

ggplot(age_cancer, aes(x=age)) + 
  geom_density(fill="#E0B0BC", alpha=0.5) +
  theme_wsj()

ggplot(age_no_cancer, aes(x=age)) + 
  geom_density(fill="#E0B0BC", alpha=0.5) +
  theme_wsj()

age_cancer$age %<>% 
  levels(.) %>% 
  as.numeric()

age_no_cancer$age %<>% 
  levels(.) %>% 
  as.numeric()


#linear plot  
ggplot(age_cancer, aes(x=age, y=num)) + 
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(aes(group=1)) +
  geom_point()

ggplot(age_no_cancer, aes(x=age, y=num)) + 
  geom_area(fill="#BAD252", alpha=0.5) +
  geom_line(aes(group=1)) +
  geom_point()

g <- ggplot() + 
  geom_area(data=age_no_cancer, aes(x=age, y=freq, fill="NoCancer"), alpha=0.5, colour="#BAD252") +
  geom_line(data=age_no_cancer, aes(x=age, y=freq, group=1)) +
  geom_area(data=age_cancer, aes(x=age, y=freq, fill="LungCancer"), alpha=0.5, colour="#ED5D47") +
  geom_line(data=age_cancer, aes(x=age, y=freq, group=1)) +
  theme_solarized() + 
  labs(fill="") +
  scale_x_continuous(breaks=seq(37, 67, 1)) +
  scale_y_continuous(breaks=seq(1, 12, 2))

ggplotly(g)

