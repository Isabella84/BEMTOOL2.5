# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


utility_params=read.csv(paste(getwd(), "/src/mcda/Utility_params.csv", sep=""),sep=';',header=TRUE)  # Read the .csv file containing weights  

u_gva_mey<-as.numeric(as.character(utility_params$Value[1]))
u_gva_0.5mey<-as.numeric(as.character(utility_params$Value[2]))

#u_roi_mey<-as.numeric(as.character(utility_params$Value[3]))
#u_roi_0.5mey<-as.numeric(as.character(utility_params$Value[4]))

u_rber_1<-as.numeric(as.character(utility_params$Value[3]))
u_rber_1.5<-as.numeric(as.character(utility_params$Value[4]))

u_empl_ce<-as.numeric(as.character(utility_params$Value[5]))
u_empl_0.5ce<-as.numeric(as.character(utility_params$Value[6]))

u_wage_mnw<-as.numeric(as.character(utility_params$Value[7]))

u_ssb_0.2<-as.numeric(as.character(utility_params$Value[8]))
u_ssb_msy<-as.numeric(as.character(utility_params$Value[9]))

u_f_msy<-as.numeric(as.character(utility_params$Value[10]))
u_f_2msy<-as.numeric(as.character(utility_params$Value[11]))

u_y_msy<-as.numeric(as.character(utility_params$Value[12]))
u_y_0.5msy<-as.numeric(as.character(utility_params$Value[13]))

u_d_0.25<-as.numeric(as.character(utility_params$Value[14]))
u_d_0.5<-as.numeric(as.character(utility_params$Value[15]))


GVA_or_ROI_or_PROFITS=as.character(utility_params$Value[16])
last_values=as.numeric(as.character(utility_params$Value[17]))
