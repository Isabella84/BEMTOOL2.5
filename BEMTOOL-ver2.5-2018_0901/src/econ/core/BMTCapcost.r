# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool capital costs component
# Author: Paolo Accadia

BMTCapcost<- function(option, Flyear, ccmat, currenttime, n_fleet) {

  if (option[5] == 1) { # Birdmod
    for (i in 1:n_fleet) {
    
      bmtcapitalcost_deprec_COL1 <-  as.numeric(as.character(ccmat[1,i]))
      bmtcapitalcost_interest_COL2 <- as.numeric(as.character(ccmat[2,i]))
      bmtcapitalcost_curr_gt <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@GT.annual  ))
      
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation <- bmtcapitalcost_deprec_COL1 * bmtcapitalcost_curr_gt
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest <- bmtcapitalcost_interest_COL2 * bmtcapitalcost_curr_gt
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest)), na.rm=TRUE)
   
    }
  }
  
  if (option[5] == 2) { # Mefisto
    for (i in 1:n_fleet) {
    
      bmtcapitalcost_deprec_COL1 <-  as.numeric(as.character(ccmat[1,i]))
      bmtcapitalcost_interest_COL2 <- as.numeric(as.character(ccmat[2,i]))
      bmtcapitalcost_curr_capital <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@capital.value  ))
      
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation <- bmtcapitalcost_deprec_COL1 * bmtcapitalcost_curr_capital
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest <- bmtcapitalcost_interest_COL2 * bmtcapitalcost_curr_capital
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest)), na.rm=TRUE)
      
    } 
  }
  
  if (option[5] == 3) { # Fishrent     
    for (i in 1:n_fleet) {
    
     bmtcapitalcost_COL1 <- as.numeric(as.character(ccmat[1,i])) 
     bmtcapitalcost_curr_vessel <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual  ))
    
      Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost <- bmtcapitalcost_COL1 * bmtcapitalcost_curr_vessel
    }  
  }


  return(Flyear)
}
