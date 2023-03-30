#Boxplots
#In and Out Temp

Box_In_Out <- BMPP_2022_df %>% 
  filter(Station == "Intake" | Station == "Outfall") %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = Station, y = `Temperature (Celsius)`, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Mean Temperature (°C)", title =  "A)")+
  theme(legend.position = "none")


Box_In_Out

##Box all sites

Box_sites <- BMPP_2022_df %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = reorder(Station,`Temperature (Celsius)` ), y = `Temperature (Celsius)`, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set3")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Mean Temperature (°C)", title =  "A)")+
  theme(legend.position = "none")

Box_sites

##############Box sal 

Box_In_Out_Sal <- BMPP_2022_df %>% 
  filter(Station == "Intake" | Station == "Outfall") %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = Station, y = `Temperature (Celsius)`, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Mean Salinity (ppt)", title = "B)" )+
  theme(legend.position = "none")


Box_In_Out_Sal
#####Box sal all 
Box_sites_Sal <- BMPP_2022_df %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = reorder(Station,`Temperature (Celsius)` ), y = `Temperature (Celsius)`, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set3")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Mean Salinity (ppt)", title = "B")+
  theme(legend.position = "none")

Box_sites_Sal


####Box Kd all
Box_sites_Kd <- Kd_PAR %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = reorder(Station,Kd ), y = Kd, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set3")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Kd", title = "B)")+
  theme(legend.position = "none")

Box_sites_Kd

###Kd out and intake
Box_in_out_Kd <- Kd_PAR %>% 
  filter(Station == "Intake" | Station == "Outfall") %>% 
  mutate(Station = as.factor(Station)) %>% 
  ggplot(aes(x = reorder(Station,Kd ), y = Kd, fill = Station))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  geom_jitter(alpha = 0.05)+
  theme_classic()+
  labs(x = "Stations", y = "Kd", title = "A)")+
  theme(legend.position = "none")

Box_in_out_Kd

