# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


weights=read.csv(paste(getwd(), "/src/mcda/Weights.csv", sep=""),sep=';',header=TRUE)  # Read the .csv file containing weights  


w=weights$Value
if(sum(w)!=1) w=w/sum(w) else w=w 
names(w)=weights$Name        # Check if weights sum to 1, otherwise normalize
k_GVA_ROI_PROFITS=as.numeric(as.character(w[1]))
k_RBER=as.numeric(as.character(w[2]))
k_WAGE=as.numeric(as.character(w[3]))
k_EMPL=as.numeric(as.character(w[4]))
k_SSB=as.numeric(as.character(w[5]))
k_F=as.numeric(as.character(w[6]))
k_Y=as.numeric(as.character(w[7]))
k_D=as.numeric(as.character(w[8]))
 
