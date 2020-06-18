# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool variable costs component
# Author: Paolo Accadia

BMTVarcost <- function(option, Flyear, vcmat, vcvec , currenttime, n_fleet) {



  if (option[[2]] == 1) { # Birdmod    
    for (i in 1:n_fleet) {
    
       bmtvarcost_fuel_COL1 <- as.numeric(as.character(vcmat[1,i]))
       bmtvarcost_commercial_COL2 <- as.numeric(as.character(vcmat[2,i]))
       bmtvarcost_other_COL3 <-  as.numeric(as.character(vcmat[3,i]))
       bmtvarcost_curr_days <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
       bmtvarcost_curr_landings <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings ))
       bmtlabour_sorting_coeff_COL6 <- as.numeric(as.character(discard_coeff_mat[i,currenttime-simperiod]))    
    
          if ( bmtlabour_sorting_coeff_COL6 == -1 ) {
      #      if ( is.na(bmtlabour_sorting_coeff_COL6) ) {
             bmtlabour_LANDING_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight )) , na.rm=T)
            bmtlabour_DISCARD_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight )) , na.rm=T)
            
            bmtlabour_sorting_coeff_COL6 <-  ifelse(is.na(bmtlabour_DISCARD_target), 0, bmtlabour_DISCARD_target/(bmtlabour_DISCARD_target + bmtlabour_LANDING_target) )    
      }
    
    
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost <- bmtvarcost_fuel_COL1 * bmtvarcost_curr_days
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_landings 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost <- bmtvarcost_other_COL3 * bmtvarcost_curr_days
        
   Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost <-  Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost +  bmtlabour_sorting_coeff_COL6 * Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost # adjustment with the sorting cefficient   
 
  Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost)), na.rm=TRUE)
    
 
     print(paste("COMMERCIAL COSTS ---------- YEAR:", currenttime-simperiod, "FLEET:", i, " coeff ==== ", bmtlabour_sorting_coeff_COL6))   
    
		for (PERC in c(1:5)) {
		bmtvarcost_curr_landings <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings.CI.perc[PERC] ))
 
 		   if ( bmtlabour_sorting_coeff_COL6 == -1 ) {
#      if ( is.na(bmtlabour_sorting_coeff_COL6) ) {
             bmtlabour_LANDING_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,] )) , na.rm=T)
            bmtlabour_DISCARD_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC,] )) , na.rm=T)
            bmtlabour_sorting_coeff_COL6 <-  ifelse(is.na(bmtlabour_DISCARD_target), 0, bmtlabour_DISCARD_target/(bmtlabour_DISCARD_target + bmtlabour_LANDING_target) )    
      }
 
 Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC] <- bmtvarcost_fuel_COL1 * bmtvarcost_curr_days
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC] <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_landings 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC] <- bmtvarcost_other_COL3 * bmtvarcost_curr_days
      
        Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC] <-   Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC] + bmtlabour_sorting_coeff_COL6 * Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC]     
      
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC])), na.rm=TRUE)
				
		
		}
		
		}
  }
  
  if (option[[2]] == 2) { # Mefisto
    for (i in 1:n_fleet) {
    
      bmtvarcost_fuel_COL1 <- as.numeric(as.character(vcmat[1,i]))
      bmtvarcost_commercial_COL2 <- as.numeric(as.character(vcmat[2,i]))
      bmtvarcost_other_COL3 <-  as.numeric(as.character(vcmat[3,i]))
      bmtvarcost_ice_COL4 <-  as.numeric(as.character(vcmat[4,i]))
       bmtvarcost_curr_days <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
       bmtvarcost_curr_revenues <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues ))
      
      bmtvarcost_fuel_timeseries <- as.numeric(as.character(vcvec[currenttime-simperiod]))
                
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost <- bmtvarcost_fuel_timeseries *  bmtvarcost_fuel_COL1 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_revenues
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost <- bmtvarcost_ice_COL4 * bmtvarcost_curr_days + bmtvarcost_other_COL3 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost <- sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost)),  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost)), na.rm=TRUE)
    
		
				for (PERC in c(1:5)) {
	  bmtvarcost_curr_revenues <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC] ))
                
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC] <- bmtvarcost_fuel_timeseries *  bmtvarcost_fuel_COL1 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC] <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_revenues
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC] <- bmtvarcost_ice_COL4 * bmtvarcost_curr_days + bmtvarcost_other_COL3 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC] <- sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC])),  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC])), na.rm=TRUE)					
				}

		
		} 
  }
  
  if (option[[2]] == 3) { # Fishrent   
    for (i in 1:n_fleet) {
    
          bmtvarcost_fuel_timeseries <- as.numeric(as.character(vcvec[currenttime-simperiod]))
          bmtvarcost_fuel_COL1 <- as.numeric(as.character(vcmat[1,i]))
          bmtvarcost_commercial_COL2 <- as.numeric(as.character(vcmat[2,i]))
          bmtvarcost_curr_days <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
           bmtvarcost_curr_revenues <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues ))
                 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost <- bmtvarcost_fuel_timeseries * bmtvarcost_fuel_COL1 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost <- 0  
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_revenues
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost <- sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost)),  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost)), na.rm=TRUE)
    
		for (PERC in c(1:5)) {
           bmtvarcost_curr_revenues <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC] ))
                 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC] <- bmtvarcost_fuel_timeseries * bmtvarcost_fuel_COL1 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC] <- 0
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC] <- bmtvarcost_commercial_COL2 * bmtvarcost_curr_revenues 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC] <- sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC])),  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC])), na.rm=TRUE)
		
		}
		
		
		}  
  }
  
  
  if (option[[2]]==4) { # Bemmfish
    for (i in 1:n_fleet) {
    
              bmtvarcost_fuel_COL1 <- as.numeric(as.character(vcmat[1,i]))
              bmtvarcost_curr_days <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
                        
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost <- 0 # bmtvarcost_fuel_COL1 * bmtvarcost_curr_days 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost <- 0 
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost <- 0
      Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost <- bmtvarcost_fuel_COL1 * bmtvarcost_curr_days  
      #sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost)),  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost)), na.rm=TRUE)
    
    for (PERC in c(1:5)) {
         Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC] <- bmtvarcost_fuel_COL1 * bmtvarcost_curr_days  
    
    }
    
    }
  }

  return(Flyear)
}
