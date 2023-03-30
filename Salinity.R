#Salinity


Sal_out_in = BMPP_2022_df %>% 
  #filter(`Depth (Meter)`<1) %>% 
  filter(Station=="Intake"|Station=="Outfall") %>% 
  group_by(month, Station) %>% 
  mutate(Mean_Sal = mean(`Salinity (Practical Salinity Scale)`),
         ymin = min(`Salinity (Practical Salinity Scale)`), 
         ymax = max(`Salinity (Practical Salinity Scale)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_Sal,color = Station))+
  geom_line(aes(x = month, y = Mean_Sal,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_Sal, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dak2")+
  labs(x = "Months", y = "Mean Salinity(ppt)")

Sal_out_in



Sal_all_sites = BMPP_2022_df %>% 
  group_by(month, Station) %>% 
  mutate(Mean_Sal = mean(`Salinity (Practical Salinity Scale)`),
         ymin = min(`Salinity (Practical Salinity Scale)`), 
         ymax = max(`Salinity (Practical Salinity Scale)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_Sal,color = Station))+
  geom_line(aes(x = month, y = Mean_Sal,color = Station))+
  geom_ribbon(aes(x = month, y = Mean_Sal, ymin = ymin, ymax = ymax, fill = Station), alpha = 0.15)+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  labs(x = "Months", y = "Mean Salinity(ppt)")


Sal_all_sites

highlight_in_out_Sal =  BMPP_2022_df %>% 
  group_by(month, Station) %>% 
  mutate(Mean_Sal = mean(`Salinity (Practical Salinity Scale)`),
         ymin = min(`Salinity (Practical Salinity Scale)`), 
         ymax = max(`Salinity (Practical Salinity Scale)`)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = Mean_Sal,color = Station))+
  geom_line(aes(x = month, y = Mean_Sal,color = Station))+
  theme_classic()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_manual(values = c("grey", "grey", "grey","grey","grey","grey","grey","Red", "blue"))+
  labs(x = "Months", y = "Mean Salinity(ppt)")

highlight_in_out_Sal

#change salinity because?
#Wet season - April to november 
#DRY- December to March  



