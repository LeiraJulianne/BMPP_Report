#Analisis estadistico 
library(FSA)

#Temperature
model_nonpar_all_temp = kruskal.test(`Temperature (Celsius)`~Station, data = BMPP_2022_df)

model_dun_temp = dunnTest(`Temperature (Celsius)`~Station,data = BMPP_2022_df,
                     method = "holm")
model_dun_temp


#Out and in 
BMPP_2022_in_out_ = BMPP_2022_df %>% 
  filter( Station == "Outfall" | Station == "Intake") 



wilcox.test(`Temperature (Celsius)`~ Station, data = BMPP_2022_in_out_)
wilcox.test(`Salinity (Practical Salinity Scale)`~ Station, data = BMPP_2022_in_out_)

#Salinity
model_nonpar_all_sal = kruskal.test(`Salinity (Practical Salinity Scale)`~Station, data = BMPP_2022_df)

model_dun_sal = dunnTest(`Salinity (Practical Salinity Scale)`~Station,data = BMPP_2022_df,
                     method = "holm")
model_dun_sal

###Kd all 
model_nonpar_all_Kd= kruskal.test(Kd~Station,data = Kd_PAR)
model_dun_all_Kd = dunnTest(Kd~Station,data = Kd_PAR,
                         method = "holm")
model_dun_all_Kd


###Kd in and out
BMPP_2022_in_out_KD = Kd_PAR %>% 
  filter( Station == "Outfall" | Station == "Intake") 
wilcox.test(Kd~ Station, data = BMPP_2022_in_out_KD)

BMPP_2022_in_out_KD

