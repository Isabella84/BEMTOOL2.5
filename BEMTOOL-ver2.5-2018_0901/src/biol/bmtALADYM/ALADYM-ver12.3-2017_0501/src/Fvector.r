# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




Fvector<-function(F_vector_input,loca_Length_Average)   {
F_matrix = matrix (nrow=length(loca_Length_Average),ncol=1)
nyears = length(F_vector_input) -1   

    # età 0, in cui dobbiamo tener conto del tr
  #  F_matrix[1:(12-tr+1)] =  F_vector[1]
   F_matrix[1:(12-modulo(INP$tr,12)) ] =  F_vector_input[1]
   
    # altri anni
    for (i in 1:nyears) {                       # (12-tr+2):length(loca_Length_Average)
    # F_matrix[((12-tr+2)+(i-1)*12):((12-tr+2)+(i*12)-1)] =  F_vector[i+1]
      F_matrix[((12-modulo(INP$tr,12))+1+(i-1)*12):((12-modulo(INP$tr,12))+1+(i*12))] =  F_vector_input[i+1]
    }
    
return(F_matrix)
}
