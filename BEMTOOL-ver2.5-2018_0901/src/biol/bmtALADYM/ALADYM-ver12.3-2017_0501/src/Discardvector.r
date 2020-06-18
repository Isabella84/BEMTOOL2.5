# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# THIS FUNCTION TRANSFORMS THE PROPORTION OF DISCARDED INDIVIDUALS BY AGE (IN YEARS)IN MONTHLY VECTOR 

Discardvector<-function(D_Vector,loca_Length_Average,tr)   {

if (showCompTime)  {
Discardvector_ptm <- proc.time()  
}

D_matrix = matrix (nrow=length(loca_Length_Average),ncol=1)
nyears = length(D_Vector) -1   

    # età 0, in cui dobbiamo tener conto del tr
  #  F_matrix[1:(12-tr+1)] =  F_vector[1]
   D_matrix[1:(12-modulo(tr,12)) ] =  D_Vector[1]
   
    # altri anni
    for (i in 1:nyears) {                       # (12-tr+2):length(loca_Length_Average)
      D_matrix[((12-modulo(tr,12))+1+(i-1)*12):((12-modulo(tr,12))+1+(i*12))] =  D_Vector[i+1]
    }
    
 if (showCompTime)  {
 proc_ <- proc.time()
# SIMULATION_EXPLOITED_ptm <- proc.time()
print(paste("Discardvector [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-Discardvector_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - SIMULATION_EXPLOITED_ptm, quote=F ) 
rm(Discardvector_ptm)
}
    
return(D_matrix)
}
