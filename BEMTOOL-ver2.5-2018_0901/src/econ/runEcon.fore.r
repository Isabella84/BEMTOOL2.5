# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# runEcon.r - Bemtool economic indicators in the simulation period
# Author: Paolo Accadia

 runEcon.fore <- function(Fyear, year_to_be_updated) {
  
 if (FALSE) {
 year_to_be_updated =65
 currenttime = 65
 Fyear = Fleetyear
  Flyear = Fleetyear
  
  }
      
  print(paste("Running economic simulation for",years.forecast[year_to_be_updated-simperiod], "..."), quote=F)   

#############################################################################
# FIRST YEAR OF SIMULATION - FIRST YEAR AFTER THE BASE YEAR
##### TO BE INCLUDED IN A CYCLE OF "FOR" FOR THE SIMULATION PERIOD ############

# time <- yy

# import of capacity indicators by fleet segment from harvest rules module
### number of vessels depends also on investments: Nt = Nt-1 + It-1 CONSISTENCY TO BE CHECKED
# estimate of capital value and employment as well
    
    for (i in 1:n_fleet) {  
#      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@capital.value <- as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@capital.value))-as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@capital.cost[2]))+as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@investment[2]))
# -----------------------------------------------------------------------------
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@capital.value <- sum(as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@capital.value)), as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@investment$value)) , na.rm=T)
# -----------------------------------------------------------------------------
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@employment <- as.numeric(as.character(eimat[7,i])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@VESSELS.annual))
# -----------------------------------------------------------------------------      
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@MAXDAYS.average.annual <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[i]]@MAXDAYS.average.annual)) 
# -----------------------------------------------------------------------------
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@technology <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[i]]@technology)) 

    for (PERC in c(1:5)) {
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@capital.value.CI.perc[1,PERC] <- sum(as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@capital.value.CI.perc[1,PERC] )), as.numeric(as.character(Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@ investment.CI.perc$value[PERC])) , na.rm=T)		
# -----------------------------------------------------------------------------
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@employment.CI.perc[1, PERC] <- as.numeric(as.character(eimat[7,i])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@VESSELS.annual))
		}
		
		}

    # estimate of price by using the function BMTPrice for all combination stock-fleet
    Fyear <- BMTPrice(option, Fyear, pmat1, pmat2, pmat3, year_to_be_updated, n_fleet, m_stock)
   
     # estimate of price fro discard in case of landing obligation by using the function BMTPrice for all combination stock-fleet
    Fyear <- BMTPrice_discard(option_discard_price, Fyear, year_to_be_updated, n_fleet, m_stock)

# ------------------------------------------------------------------------------------------------ ------------------------    
# ------------------------------------------------------------------------------------------------ REVENUES CALCULATION  
# ------------------------------------------------------------------------------------------------ ------------------------    
    
    for (i in 1:n_fleet) {
    
      for (j in 1:m_stock) {
      
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing[j] <-   as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@price[j]))   
            
if ( as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") { 
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard[j] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight[j])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@price.landed_discard[j]))      
      } else {
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard[j] <- 0   
      } 
        
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues[j] <- sum(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing[j],  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard[j], na.rm=T)  
	
  # CI ********************************************	
		 for (PERC in c(1:5)) {
		 
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,j] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@price.CI.perc[PERC, j]))		 
		 
if ( as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") {
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,j] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC,j])) * as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC, j]))		  
} else {
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,j] <- 0
    }  
  
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.CI.perc[PERC,j] <- sum(  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,j] ,   Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,j] , na.rm=T)
			}		

}

Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landed_discard  <- sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard)) , na.rm=T) 
	  
	  
	  
equation_type_COL1 <- as.numeric(as.character(eimat[1,i]))

	 if (equation_type_COL1 == 1) {
	 # read col 2 and 3
	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))

	Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings  <- correction_factor_revenues_COL3 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing)) , na.rm=T) 
	  
	  } else {
	
	 coeff_factor_landing_v_COL4 <- as.numeric(as.character(eimat[4,i]))
	  coeff_factor_landing_u_COL5 <- as.numeric(as.character(eimat[5,i]))
	  
	  prev_revenues_target <-  sum(as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@revenues.landing)) , na.rm=T)
	  prev_total_revenues <-  as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@total.revenues.landings)) 
	  
	  prev_landing_target <-  sum(as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@landing.weight)) , na.rm=T)
	  prev_total_landing <-  as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@total.landings))   
	  
    price_others <- (prev_total_revenues - prev_revenues_target) / (prev_total_landing - prev_landing_target)
    
    this_year_target_land <- sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T)
    
 # option 2 : additive (u + v*C)
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings  <- sum( ( (as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings)) - this_year_target_land) * price_others) , sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing)) , na.rm=T)  , na.rm=T)

	  }
			                                                                                                                                                                                                                
    
    for (PERC in c(1:5)) {
    
    Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landed_discard.CI.perc[PERC]  <- sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,])) , na.rm=T) 
    
			 if (equation_type_COL1 == 1) {
			 
			 	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))

	Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings.CI.perc[PERC] <- correction_factor_revenues_COL3 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,])) , na.rm=T)       

	  } else {
	  
 coeff_factor_landing_v_COL4 <- as.numeric(as.character(eimat[4,i]))
	  coeff_factor_landing_u_COL5 <- as.numeric(as.character(eimat[5,i]))
	  
#	  prev_revenues_target <-  sum(as.numeric(as.character( Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,])) , na.rm=T)
#	  prev_total_revenues <-  sum(as.numeric(as.character( Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC])), - sum(as.numeric(as.character( Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,])), na.rm=T) , na.rm=T) 
#	  
#	  prev_landing_target <-  sum(as.numeric(as.character( Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)
#	  prev_total_landing <-  as.numeric(as.character( Fyear[[year_to_be_updated-1]]@fleetsegments[[i]]@total.landings.CI.perc[PERC])) 
	 
   
   	  prev_revenues_target <-  sum(as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,])) , na.rm=T)
	  prev_total_revenues <-  as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@total.revenues.landings.CI.perc[PERC])) #  sum(  ,  - sum(as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@revenues.landed_discard.CI.perc[PERC,])), na.rm=T) , na.rm=T) 
	  
	  prev_landing_target <-  sum(as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)
	  prev_total_landing <-  as.numeric(as.character( Fyear[[simperiod]]@fleetsegments[[i]]@total.landings.CI.perc[PERC]))
   
price_others <- (prev_total_revenues - prev_revenues_target) / (prev_total_landing - prev_landing_target)
      
        this_year_target_land <- sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)
    
 # option 2 : additive (u + v*C)
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings.CI.perc[PERC] <- sum( ( (as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[PERC])) - this_year_target_land) * price_others) , sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.landing.CI.perc[PERC,])) , na.rm=T)  , na.rm=T)
    
# Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC] <- sum( ( (prev_total_landing - prev_landing_target) * price_others) , sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@revenues.CI.perc[PERC,])) , na.rm=T)  , na.rm=T)
          
	  }
		
		}

    	Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues <-  sum(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings, Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landed_discard, na.rm=T)

      for (PERC in c(1:5)) {
  	Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC] <-  sum(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landings.CI.perc[PERC], Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.revenues.landed_discard.CI.perc[PERC], na.rm=T)    
      }

		}    
  



    # estimate of variable costs by using the function BMTVarcost for all fleet segments
    Fyear <- BMTVarcost(option, Fyear, vcmat, vcvec , year_to_be_updated, n_fleet)
    

    # estimate of fixed costs by using the function BMTFixcost for all fleet segments
    Fyear <- BMTFixcost(option, Fyear, fcmat, year_to_be_updated, n_fleet, tsmat3)    
    
    
    # estimate of labour costs by using the function BMTLabcost for all fleet segments
    Fyear <- BMTLabcost(option, Fyear, lcmat, year_to_be_updated, n_fleet)
    
    
    # estimate of capital costs by using the function BMTCapcost for all fleet segments
    Fyear <- BMTCapcost(option, Fyear, ccmat, year_to_be_updated, n_fleet)
      
    # estimate of final economic and social variables and indicators
    Fyear <- BMTEconind(Fyear, tsmat1, tsmat2, eimat, year_to_be_updated, n_fleet) 
    
    return(Fyear)
}
