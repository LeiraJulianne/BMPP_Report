#Analisis estadistico 
library(FSA)
model_nonpar_all = kruskal.test(`Temperature (Celsius)`~Station, data = BMPP_2022_df)

model_dun = dunnTest(`Temperature (Celsius)`~Station,data = BMPP_2022_df,
                     method = "holm")
model_dun


#Out and in 
BMPP_2022_in_out_ = BMPP_2022_df %>% 
  filter( Station == "Outfall" | Station == "Intake") 



wilcox.test(`Temperature (Celsius)`~ Station, data = BMPP_2022_in_out_)


