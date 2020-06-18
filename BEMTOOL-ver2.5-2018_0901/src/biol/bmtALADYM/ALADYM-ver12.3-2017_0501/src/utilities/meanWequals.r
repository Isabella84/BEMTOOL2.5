# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




meanWequals <- function(para_inVect, para_vectLength, para_pointsMean) {

#
# Initialize the vectors
#  questa funzione calcola le medie annuali di una determinata variabile

loca_vect_len            <- para_vectLength / para_pointsMean + 1
loca_vect                <- vector(mode="numeric", length=loca_vect_len)

  loca_vect[1] <- para_inVect[1]

  for(loca_i in (2 : loca_vect_len)) {
    loca_jstart = 2 + para_pointsMean * (loca_i - 2)
    loca_jstop  = 2 + para_pointsMean * (loca_i - 1) - 1
    
    if(all(is.na(para_inVect[loca_jstart:loca_jstop])))
    {loca_vect[loca_i]="NA"}  else{ 
    loca_vect[loca_i] <-  mean(na.omit(para_inVect[loca_jstart:loca_jstop]))
    }
  }

  return(loca_vect[2:loca_vect_len])
}
