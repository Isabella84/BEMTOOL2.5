# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




RFSS <- function(S_unit, para_SS_Population, para_RLt, para_RLa, para_RLb, para_RLc, para_R) {

if (FALSE) {
S_unit =INP$S_unit
para_SS_Population= loca_SS_Population
para_RLt = INP$FRLt
para_RLa= INP$FRLa
para_RLb = INP$FRLb
para_RLc=INP$FRLc
para_R = NA #INP$Recruits[loca_irun + 1]

}



  loca_temp <- 0
  
   if(INP$S_unit==1){      
   para_SS_Population <-    para_SS_Population/1000000 # biomassa in tons
   } else {
   para_SS_Population <-    para_SS_Population  /1000  # la relazione S-R vuole i numeri in migliaia    
   }
   
  if(para_RLt == 1) {
     loca_temp <- para_SS_Population / (para_RLa + para_RLb * para_SS_Population)  * 1000         # il numero di reclute è in migliaia, devo trasformarlo il numeri assoluti
              
  } else if (para_RLt == 2) {
    loca_temp <- para_RLa * para_SS_Population * exp(-para_RLb * para_SS_Population) * 1000                  # il numero di reclute è in migliaia       
  
  } else if (para_RLt == 3) {
    loca_temp <- para_RLa * para_SS_Population / (1 + (para_SS_Population / para_RLc)^para_RLb)     * 1000       # il numero di reclute è in migliaia
  
  } else if (para_RLt == 4) {
    loca_temp <- para_R  * 1000       # il numero di reclute è in migliaia
  
  } else if(para_RLt == 5) {
    loca_temp <- para_RLa * min(para_SS_Population, para_RLb)  * 1000       # il numero di reclute è in migliaia
  
  } else if(para_RLt == 6) {
    if(para_SS_Population <= (para_RLb * (1 - para_RLc))) {
      loca_temp <- para_RLa * para_SS_Population   * 1000       # il numero di reclute è in migliaia
    } else if (para_SS_Population >= (para_RLb * (1 + para_RLc))) {
      loca_temp <- para_RLa * para_RLb   * 1000       # il numero di reclute è in migliaia
    } else {  
      #  R <- a * (SSB - (SSB - b * (1 - c))^2 / (4 * c * b))     
     loca_temp <- para_RLa * (para_SS_Population - (para_SS_Population - para_RLb * (1 - para_RLc))^2 / (4 * para_RLc * para_RLb))   * 1000       # il numero di reclute è in migliaia
    }
  }
  # print(loca_temp)
  return(loca_temp)
}
