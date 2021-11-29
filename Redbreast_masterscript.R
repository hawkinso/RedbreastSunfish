# Rebreast project 2021 
# Date: 11/23/2021 
# Author(s): Olivia H Hawkins 
# Goals: clean and summarize data,statistical assumptions, transformations 

# Git hub repository 
browseURL("https://github.com/hawkinso/RedbreastSunfish.git")

# Read in data 
data <- read.csv("RedBreast_2021.csv")

# Subset data that will be used in analysis ----
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

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
# Normality, homogeneity of variance, extreme outliers,independence of observations
# Independence of observations is not met as each individual is sampled 20 times 

## Normality 
shapiro.test(all.data$PG[all.data$Individual=="LAUR01"])

for(i in 1:ncol(all.data[,3:18])) {
  ddply(.data=all.data,.variables = c("Individual"),summarise,W=shapiro.test(as.numeric(i))[1],p=shapiro.test(as.numeric(i))[2])
  }

