# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool fixed costs component
# Author: Paolo Accadia

BMTFixcost <- function(option, Flyear, fcmat, currenttime, n_fleet, tsmat3) {

  if (option[4] == 1) { # Birdmod
    for (i in 1:n_fleet) {
    
    bmtfixedcost_maintenance_COL1 <- as.numeric(as.character(fcmat[1,i]))
    bmtfixedcost_other_COL2 <- as.numeric(as.character(fcmat[2,i]))
    bmtfixedcost_curr_GT <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@GT.annual ))
    bmtfixedcost_curr_newequip_cost <- as.numeric(as.character(tsmat3[i, (currenttime-simperiod)] ))
    bmtfixedcost_curr_vessel <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual ))
    
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$maint.cost <- bmtfixedcost_maintenance_COL1 * bmtfixedcost_curr_GT 
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost <- bmtfixedcost_other_COL2 * bmtfixedcost_curr_GT 
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel
       
#      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost)), na.rm=TRUE)
 Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost))
      
      
      for (PERC in c(1:5)) {

      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$maint.cost[PERC] <- bmtfixedcost_maintenance_COL1 * bmtfixedcost_curr_GT 
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$other.fix.cost[PERC] <- bmtfixedcost_other_COL2 * bmtfixedcost_curr_GT 
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC] <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel
       
#      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$other.fix.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC])), na.rm=TRUE)
 Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$other.fix.cost[PERC]))
			}
      
    
    }
  }
  
  
  if (option[4] == 2) { # Mefisto
    for (i in 1:n_fleet) {
    
    bmtfixedcost_un_maintenance_COL1 <- as.numeric(as.character(fcmat[1,i]))
    bmtfixedcost_essential_COL2 <- as.numeric(as.character(fcmat[2,i]))
    bmtfixedcost_av_maintenance_COL3 <- as.numeric(as.character(fcmat[3,i]))
    bmtfixedcost_curr_vessel <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual  ))
    bmtfixedcost_prev_profit <- as.numeric(as.character( Flyear[[currenttime-1]]@fleetsegments[[i]]@profit ))
    bmtfixedcost_curr_newequip_cost <- as.numeric(as.character(tsmat3[i, (currenttime-simperiod)] )) 
    
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$ess.cost <- bmtfixedcost_essential_COL2 * bmtfixedcost_curr_vessel
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$avoid.main.cost <- min( bmtfixedcost_av_maintenance_COL3 * bmtfixedcost_curr_vessel , max( bmtfixedcost_av_maintenance_COL3 * bmtfixedcost_curr_vessel + bmtfixedcost_prev_profit, 0) ) 
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$unavoid.main.cost <- bmtfixedcost_un_maintenance_COL1 * bmtfixedcost_curr_vessel 
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel 
      
#Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$ess.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost)), na.rm=TRUE)   
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$ess.cost))
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$maint.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$avoid.main.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$unavoid.main.cost)), na.rm=TRUE)   
    
    
      for (PERC in c(1:5)) {

    bmtfixedcost_prev_profit <- as.numeric(as.character( Flyear[[currenttime-1]]@fleetsegments[[i]]@profit.CI.perc[PERC] ))
    
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$ess.cost[PERC] <- bmtfixedcost_essential_COL2 * bmtfixedcost_curr_vessel
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$avoid.main.cost[PERC] <- min( bmtfixedcost_av_maintenance_COL3 * bmtfixedcost_curr_vessel , max( bmtfixedcost_av_maintenance_COL3 * bmtfixedcost_curr_vessel + bmtfixedcost_prev_profit, 0) ) 
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$unavoid.main.cost[PERC] <- bmtfixedcost_un_maintenance_COL1 * bmtfixedcost_curr_vessel 
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC] <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel 
      
#Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$ess.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC])), na.rm=TRUE) 
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$ess.cost[PERC]))
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$maint.cost[PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$avoid.main.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$unavoid.main.cost[PERC])), na.rm=TRUE)       
      }
    
    
    
    } 
  }
  
  if (option[4] == 3) { # Fishrent     
    for (i in 1:n_fleet) {
    
      bmtfixedcost_total_COL1 <- as.numeric(as.character(fcmat[1,i]))
      bmtfixedcost_curr_vessel <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual  ))
      bmtfixedcost_curr_newequip_cost <- as.numeric(as.character(tsmat3[i, (currenttime-simperiod)] ))
					    
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel        
#Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- sum(bmtfixedcost_total_COL1 * bmtfixedcost_curr_vessel ,  Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost, na.rm=T)
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost <- sum( bmtfixedcost_total_COL1 * bmtfixedcost_curr_vessel , Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost, na.rm=T)

Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost <- sum( bmtfixedcost_total_COL1 * bmtfixedcost_curr_vessel , Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$new.equipment.cost, na.rm=T)

    for (PERC in c(1:5)) {
					    
      Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC] <- bmtfixedcost_curr_newequip_cost * bmtfixedcost_curr_vessel        
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] <- sum(bmtfixedcost_total_COL1 * bmtfixedcost_curr_vessel ,  Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC], na.rm=T)	
Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$other.fix.cost[PERC] <- sum(bmtfixedcost_total_COL1 * bmtfixedcost_curr_vessel ,  Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$new.equipment.cost[PERC], na.rm=T)	

		}
		
		
		}  
  }

  return(Flyear)
}
