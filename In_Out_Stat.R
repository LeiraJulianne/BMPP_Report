#load required libraries
library(tidyverse)
library(nlme)
library(performance)

data=read_csv("BMPP_2022_df_2.csv")


colnames(data)=c("station","year","month","day","pressure","depth","temp","conductivity","spec_cond","sal","sound_v","dens")


View(data)

#filter for only the intake and outfall data
in_out_data=data %>% 
  filter(station=="Intake"|station=="Outfall")


View(in_out_data)


#linear mixed effects model of predicting temperature by station with random slopes and intercepts by sample event
modelt=lme(temp~station,random=~station|month,data=in_out_data,method="ML",na.action=na.omit)

#check model summary, anova, and model performance
summary(modelt) #there is no detectable difference between the intake and outfall station temperatures
anova(modelt) #there is no detectable difference between the intake and outfall station temperatures
check_model(modelt) #residuals have a non-normal tail so parametric stats would likely be better


#linear mixed effects model of predicting temperature by station with random slopes and intercepts by sample event
models=lme(sal~station,random=~1|month,data=in_out_data,method="ML",na.action=na.omit)

#check model summary, anova, and model performance
summary(models) #station outfall salinity is 0.22±0.03 psu higher than intake salinity
anova(models) #there are detectable differences between intake and outfall salinity
check_model(models) #residuals have non-normal tails so parametric stats would likely be better
