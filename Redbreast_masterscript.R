# Rebreast project 2021 
# Date: 11/23/2021 
# Author(s): Olivia H Hawkins 
# Goals: clean and summarize data,statistical assumptions, transformations 

# Git hub repository 
browseURL("https://github.com/hawkinso/RedbreastSunfish.git")

# Load in libraries 
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rstatix)
library(car)
library(plyr)
library(reshape2)
library(lmer4)

# Read in data 
data <- read.csv("RedBreast_2021.csv")

# Subset data that will be used in analysis ----
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)
  

# Make sure data is stored as data frame 
as.data.frame(all.data)

# Check out data structure 
str(all.data)

# Get group means and sd for each individual ----
means <- all.data %>%
  get_summary_stats()

# Get standard length (mean +/- SD)
mean(all.data$SL) #9.80
sd(all.data$SL) # 0.92

# Write to .csv 
write_csv(means,file = "Redbreast_summarystats_2021.csv",append = TRUE)

# Check assumptions ----
# Normality, homogeneity of variance,independence of observations
# Independence of observations is not met as each individual is sampled 20 times 


# Check normality with Shapiro-Wilk test 
## By individuals 
sw <- ddply(.data=all.data, .variables=c("Individual"),numcolwise(shapiro.test))
sw <- sw[-c(3:4,7:8,11:12,15:16,19:20),]

sw.results <- gather(data = sw,key = Variable, value=Results,2:18) # Looks like some fish do not meet normality

# Visualize normality by individual
ggqqplot(all.data, x = "PG",
         color = "Individual",facet.by="Individual") # some not
ggqqplot(all.data, x = "TTO",
         color = "Individual",facet.by = "Individual") # some not
ggqqplot(all.data, x = "TTC",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROT",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROTVEL",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") # ok
ggqqplot(all.data, x = "VELPG",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "maxVEL",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ACCPG",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "AI",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ingested_volume",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "PPDiopen",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") # pretty good 
ggqqplot(all.data, x = "VELpreycapture",
         color = "Individual",facet.by = "Individual") # pretty good

# Identify outliers 

# Individual 1 
Fish.1 <- all.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% identify_outliers(PG)
Fish.1 %>% identify_outliers(TTO)
Fish.1 %>% identify_outliers(TTC)
Fish.1 %>% identify_outliers(PPROT)
Fish.1 %>% identify_outliers(PPROTVEL)
Fish.1 %>% identify_outliers(tPPROT)
Fish.1 %>% identify_outliers(VELPG)
Fish.1 %>% identify_outliers(maxVEL)
Fish.1 %>% identify_outliers(tmaxVEL)
Fish.1 %>% identify_outliers(ACCPG)
Fish.1 %>% identify_outliers(H_L_ratio)
Fish.1 %>% identify_outliers(AI)
Fish.1 %>% identify_outliers(ingested_volume)
Fish.1 %>% identify_outliers(PPDiopen)
Fish.1 %>% identify_outliers(timeatcapture)
Fish.1 %>% identify_outliers(VELpreycapture)


# Individual 2 
Fish.2 <- all.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% identify_outliers(PG)
Fish.2 %>% identify_outliers(TTO)
Fish.2 %>% identify_outliers(TTC)
Fish.2 %>% identify_outliers(PPROT)
Fish.2 %>% identify_outliers(PPROTVEL)
Fish.2 %>% identify_outliers(tPPROT)
Fish.2 %>% identify_outliers(VELPG)
Fish.2 %>% identify_outliers(maxVEL)
Fish.2 %>% identify_outliers(tmaxVEL)
Fish.2 %>% identify_outliers(ACCPG)
Fish.2 %>% identify_outliers(H_L_ratio)
Fish.2 %>% identify_outliers(AI)
Fish.2 %>% identify_outliers(ingested_volume)
Fish.2 %>% identify_outliers(PPDiopen)
Fish.2 %>% identify_outliers(timeatcapture)
Fish.2 %>% identify_outliers(VELpreycapture)

# Individual 3
Fish.3 <- all.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% identify_outliers(PG)
Fish.3 %>% identify_outliers(TTO)
Fish.3 %>% identify_outliers(TTC)
Fish.3 %>% identify_outliers(PPROT)
Fish.3 %>% identify_outliers(PPROTVEL)
Fish.3 %>% identify_outliers(tPPROT)
Fish.3 %>% identify_outliers(VELPG)
Fish.3 %>% identify_outliers(maxVEL)
Fish.3 %>% identify_outliers(tmaxVEL)
Fish.3 %>% identify_outliers(ACCPG)
Fish.3 %>% identify_outliers(H_L_ratio)
Fish.3 %>% identify_outliers(AI)
Fish.3 %>% identify_outliers(ingested_volume)
Fish.3 %>% identify_outliers(PPDiopen)
Fish.3 %>% identify_outliers(timeatcapture)
Fish.3 %>% identify_outliers(VELpreycapture)

# Individual 4 
Fish.4 <- all.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% identify_outliers(PG)
Fish.4 %>% identify_outliers(TTO)
Fish.4 %>% identify_outliers(TTC)
Fish.4 %>% identify_outliers(PPROT)
Fish.4 %>% identify_outliers(PPROTVEL)
Fish.4 %>% identify_outliers(tPPROT)
Fish.4 %>% identify_outliers(VELPG)
Fish.4 %>% identify_outliers(maxVEL)
Fish.4 %>% identify_outliers(tmaxVEL)
Fish.4 %>% identify_outliers(ACCPG)
Fish.4 %>% identify_outliers(H_L_ratio)
Fish.4 %>% identify_outliers(AI)
Fish.4 %>% identify_outliers(ingested_volume)
Fish.4 %>% identify_outliers(PPDiopen)
Fish.4 %>% identify_outliers(timeatcapture)
Fish.4 %>% identify_outliers(VELpreycapture)

# Indvidual 5 
Fish.5 <- all.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% identify_outliers(PG)
Fish.5 %>% identify_outliers(TTO)
Fish.5 %>% identify_outliers(TTC)
Fish.5 %>% identify_outliers(PPROT)
Fish.5 %>% identify_outliers(PPROTVEL)
Fish.5 %>% identify_outliers(tPPROT)
Fish.5 %>% identify_outliers(VELPG)
Fish.5 %>% identify_outliers(maxVEL)
Fish.5 %>% identify_outliers(tmaxVEL)
Fish.5 %>% identify_outliers(ACCPG)
Fish.5 %>% identify_outliers(H_L_ratio)
Fish.5 %>% identify_outliers(AI)
Fish.5 %>% identify_outliers(ingested_volume)
Fish.5 %>% identify_outliers(PPDiopen)
Fish.5 %>% identify_outliers(timeatcapture)
Fish.5 %>% identify_outliers(VELpreycapture)

# Check homogeneity of variance with Levene's test 
leveneTest(all.data$PG~all.data$Individual)
leveneTest(all.data$TTO~all.data$Individual)
leveneTest(all.data$TTC~all.data$Individual)
leveneTest(all.data$PPROT~all.data$Individual)
leveneTest(all.data$PPROTVEL~all.data$Individual) # p < 0.0001
leveneTest(all.data$tPPROT~all.data$Individual) # p = 0.007
leveneTest(all.data$VELPG~all.data$Individual) # p = 0.0005
leveneTest(all.data$maxVEL~all.data$Individual) # p = 0.0002
leveneTest(all.data$tmaxVEL~all.data$Individual)
leveneTest(all.data$ACCPG~all.data$Individual)
leveneTest(all.data$H_L_ratio~all.data$Individual)#  p < 0.001
leveneTest(all.data$AI~all.data$Individual)
leveneTest(all.data$ingested_volume~all.data$Individual) # P < 0.0001
leveneTest(all.data$PPDiopen~all.data$Individual) # p = 0.02
leveneTest(all.data$timeatcapture~all.data$Individual) # p = 0.02
leveneTest(all.data$VELpreycapture~all.data$Individual) # p < 0.0001








# Check that size is similar among individuals ----
# We want to be sure that size does not influence any variables 
ggboxplot(all.data, x = "Individual", y = "SL", add = "point")

SL <- all.data %>%  # no extreme outliers 
  group_by(Individual) %>%
  identify_outliers(SL)

shapiro.test(all.data$SL) # p = 0.01 

ggqqplot(all.data$SL) # looks ok 

mod.SL <- lmer(PG~SL+(1|Individual),data=all.data)
summary(mod.SL)

summary(glm(PG~Individual,data=all.data))

