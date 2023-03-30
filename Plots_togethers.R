#Plots juntas

library(patchwork)


Temp_Sal_In_Out <- temp_out_in/ Sal_out_in +plot_layout(widths = 10)

Temp_Sal_In_Out

Temp_Sal_all <- temp_all_sites/ Sal_all_sites+ plot_layout(widths = 10)
Temp_Sal_all

Highlight <- highlight_in_out/ highlight_in_out_Sal
Highlight


##Box plot

Box_In_Out /Box_In_Out_Sal+ plot_layout(heights =  4)

Box_sites/Box_sites_Sal+plot_layout(widths = 20)

##Box kD
Box_in_out_Kd / Box_sites_Kd+plot_layout(widths = 20)
