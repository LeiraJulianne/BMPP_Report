### PAR-LICOR 

library(tidyverse)

#1ero Funcion 

Kd_Cal=function(z1,z2,E1,E2) {
  First=1/(z2-z1)
  Second=log(E1/E2)
  Then=First*Second 
  print(Then)
}

# 2do identificar los valores (z1,z2 ect. y crear columna de kd )
Kd_PAR = PAR %>% 
  mutate(Kd=Kd_Cal(First_meter,Second_meter,Underwater_First,Underwater_Second))

Kd_=Kd_PAR %>% 
  ggplot(aes(x = Month, y = Kd, color = Station))+
  geom_point(alpha = 0.7)+
  geom_line(alpha = 0.5)+
  theme_classic()+
  scale_x_continuous(breaks = c(0:12))

Kd_

view(Kd_PAR)

#Plot

Kd_PAR %>% 
  ggplot(aes(x = Month, y = Kd, color = Station))+
  geom_point()



kd_test = Kd_PAR %>% 
  filter(Station == 8)

kd_test %>% 
  mutate(Kd_2 = Kd_Cal(1,3,Underwater_First,Underwater_Second)) %>% 
  ggplot(aes(x = Month, y = Kd_2, color = Station))+
  geom_point()+
  geom_line()

Kd_PAR %>% 
  filter(Station == "4N", Month > 9) %>% view()
