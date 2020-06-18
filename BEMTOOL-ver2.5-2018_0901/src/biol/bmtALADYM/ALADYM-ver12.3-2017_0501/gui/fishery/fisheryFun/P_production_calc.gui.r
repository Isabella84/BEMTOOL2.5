# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




P_production_calc.gui <- function (prod_data,n) {
# gear_data=read.table(file=Gears_data,sep=";", header=TRUE)
# prod_data= read.table(prod_data_name,header=TRUE,sep=";")      
# nb_gears=2
Production =  matrix(nrow=(n),ncol=length(FLEETSEGMENTS_names))
Proportions =  matrix(nrow=(n),ncol=length(FLEETSEGMENTS_names))
for (gear in 1:length(FLEETSEGMENTS_names)) {
      P_Prod_temp = prod_data[prod_data$Gear == unique(prod_data$Gear)[gear],]
      for (loca_i in 1:(n)){
      Production[loca_i,gear]= as.numeric(as.character(P_Prod_temp$Production[loca_i]) )
      
      }
      
 }
 
for (r in 1:nrow(Production)) {
    for (c in 1:ncol(Production)) {
    Proportions[r,c]<- ifelse(!is.finite(Production [r,c]/sum(Production [r,])), 0,  Production [r,c]/sum(Production [r,]))
    } 
}

return(Proportions)
}
