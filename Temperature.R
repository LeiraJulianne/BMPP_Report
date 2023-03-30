#Leira J. Centeno
#Final Report BMPP
#March 28, 2023

library(tidyverse)
library(lubridate)
library(RColorBrewer)
#Identify up and down

#x<-January_2022[1:which.max(January_2022$`Depth (Meter)`),]
#$Cast="Up"
##1-28 down
##29-56 up  
#January_2022[1:which.max(January_2022$`Depth (Meter)`),]$Cast="Up"
#View(January_2022)
#January_2022[1:which.max(January_2022$`Depth (Meter)`),]$Cast="Down"
#


###############################
BMPP_2022_Data = rbind(January_2022,February_2022,March_2022,April_2022,May_2022,June_2022,July_2022,August_2022,October_2022_I, October_2022_II, November_2022, December_2022)
view(BMPP_2022_Data)
str(BMPP_2022_Data)
str(BMPP_2022_df)

BMPP_2022_df = BMPP_2022_Data %>% 
  rename(Date_BMPP = Date) %>% 
  mutate(Date_BMPP = as.Date(Date_BMPP)) %>% 
  separate(Date_BMPP,c("year", "month", "day")) %>% 
  mutate(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))




BMPP_2022_df



###################
#mean of temp for months 
Test <-  BMPP_2022_df %>% 
  #filter(`Depth (Meter)`<1) %>% 
  filter(Station=="Intake"|Station=="Outfall") %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`))






#PLOTTTT   
###
temp_out_in = BMPP_2022_df %>% 
  #filter(`Depth (Meter)`<1) %>% 
  filter(Station=="Intake"|Station=="Outfall") %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_temp, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  labs(x = "Months", y = "Mean Temperature (°C)")

temp_out_in

temp_all_sites = BMPP_2022_df %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_temp, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_brewer(palette="Set3")+
  scale_fill_brewer(palette="Set3")+
  labs(x = "Months", y = "Mean Temperature (°C)")


temp_all_sites

highlight_in_out =  BMPP_2022_df %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_manual(values = c("grey", "grey", "grey","grey","grey","grey","grey","Red", "blue"))+
  labs(x = "Months", y = "Mean Temperature (°C)")

highlight_in_out







#######################
In_out_df = BMPP_2022_df %>% 
  #filter(`Depth (Meter)`<1) %>% 
  filter(Station=="Intake"|Station=="Outfall") %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`)) 

all_temp_df = BMPP_2022_df %>% 
  group_by(month, Station) %>% 
  mutate(Mean_temp = mean(`Temperature (Celsius)`),
         ymin = min(`Temperature (Celsius)`), 
         ymax = max(`Temperature (Celsius)`)) 

In_out_df%>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_temp, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))






##Out and in travis 
Test %>%
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_temp, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))
####

Test %>%
  ggplot()+
  geom_point(aes(x = month, y = Mean_temp,color = Station))+
  geom_line(aes(x = month, y = Mean_temp,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_temp, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

test_lm = lm(`Temperature (Celsius)`~Station,data=Test)
summary(test_lm)


library(performance)
check_model(test_lm) #this doesn't look good


#################
BMPP_2022_df

write.csv(BMPP_2022_df, "BMPP_2022_df.csv")
view(Test)

################
#The same for sal 
#
#

ggplot(aes(x = month, y = Mean_temp`,color = Station))+
  geom_point(alpha = 0.5)+
  geom_line()


BMPP_2022_df %>% 
  ggplot(aes(x = month, y = `Temperature (Celsius)`,color = Station)) +
  geom_point(alpha = 0.5)+
  geom_line()+
  geom_ribbon(aes(x = month, y = `Temperature (Celsius)`))+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6.7,8,9,10,11,12))

head(BMPP_2022_df)



ggplot(aes(x=`Temperature (Celsius)`, y= `Depth (Meter)`))+
  geom_point(color= `Date_BMPP`)
