# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool price component
# Author: Paolo Accadia

BMTPrice<- function(option, Flyear, pmat1, pmat2, pmat3, currenttime, n_fleet, m_stock) {

if (FALSE) {
Flyear = Fleetyear
currenttime = 59
i = 1
j = 1
}

# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 1   
# --------------------------------------------------------------------------------------------------------


  if (option[[1]] == 1) {  # Birdmod 
    for (i in 1:n_fleet) {
      for (j in 1:m_stock) {   
      
      bmtprice_landing_prevyear <- as.numeric(as.character(Flyear[[currenttime - 1]]@fleetsegments[[i]]@price[j] ))
      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))

bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight[j] ))
bmtprice_landing_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@landing.weight[j] ))

Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_landing_prevyear * ( 1 + bmtprice_elastity_coeff_COL1 *( (bmtprice_landing_weight_curryear - bmtprice_landing_weight_prevyear) / bmtprice_landing_weight_prevyear) ) 

      for (PERC in c(1:5)) {	      
      bmtprice_landing_prevyear <- as.numeric(as.character(Flyear[[currenttime - 1]]@fleetsegments[[i]]@price.CI.perc[PERC, j] ))
      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))
     
       bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j] ))
       bmtprice_landing_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j] ))
      
   Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[PERC, j] <- bmtprice_landing_prevyear * ( 1 + bmtprice_elastity_coeff_COL1 *( (bmtprice_landing_weight_curryear - bmtprice_landing_weight_prevyear) / bmtprice_landing_weight_prevyear) ) 

			}
      }
    }

# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 2   
# --------------------------------------------------------------------------------------------------------    

  } else if (option[[1]] == 2) { # Mefisto
    
      for (j in 1:m_stock) {
  
 loca_INP <- get(paste("INP_", j, sep=""))                           # ADRIATIC - Population C.I. FORE HR5-SQ_2910_1 10 runs
     print( loca_INP$nruns )
 ALADYM_spe <<- j
source(paste(ALADYM_home, "/src/paths.r", sep=""))

production_table_all_runs <- suppressWarnings(try(read.table(paste(PRODUCTION_table_CI, " ",  loca_INP$nruns, " runs.csv", sep=""), header=TRUE,sep=";")) )     
      
      associated_fleetsegment_ec <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", j, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment_ec <<- associated_fleetsegment_ec[!is.na(associated_fleetsegment_ec) & associated_fleetsegment_ec!=""]
associated_fleetsegment_indices_ec <<- which(associated_fleetsegment_ec %in% BMT_FLEETSEGMENTS)   
        int_ass <- 1  
    n_fleet_for_species <- length(associated_fleetsegment_indices_ec)
    
     for (i in 1:n_fleet) {

#bmtprice_landing_meanweight_curryear <- as.numeric(as.character( Interactionsyear[[currenttime]][[j]]@interactions[[int_ass]]$landings@meanWeight )) / 1000
bmtprice_landing_meanweight_curryear <- as.numeric(as.character( Interactionsyear[[currenttime]][[j]]@interactions[[int_ass]]$landings@meanWeight )) 
bmtprice_lastSim_year <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price[j] )) 
#print(paste("Peso medio",bmtprice_landing_meanweight_curryear))
#bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight[j] ))
bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight[j] )) /1000

      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))
      bmtprice_import_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@import.weight[j] ))
      bmtprice_elastity_coeff_COL2 <-  as.numeric(as.character(pmat2[j,i]))
      bmtprice_elastity_coeff_COL3 <-  as.numeric(as.character(pmat3[j,i]))
              #  Fleetyear[[65]]@fleetsegments[[1]]@price
              #  as.numeric(as.character( Fleetyear[[simperiod]]@fleetsegments[[i]]@price.CI.perc[1,PERC] ))  
     if (bmtprice_import_weight_curryear != -1) {
         Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_lastSim_year * ( bmtprice_landing_weight_curryear ^ bmtprice_elastity_coeff_COL1)*((bmtprice_import_weight_curryear) ^ bmtprice_elastity_coeff_COL2)*( bmtprice_landing_meanweight_curryear ^ bmtprice_elastity_coeff_COL3 )
     } else {
         Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_lastSim_year * ( bmtprice_landing_weight_curryear ^ bmtprice_elastity_coeff_COL1)*( bmtprice_landing_meanweight_curryear ^ bmtprice_elastity_coeff_COL3 )
     }      
	
	
	#	for (PERC in c(1:5)) {

    bmtprice_lastSim_year <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price.CI.perc[1,1] )) 

# bmtprice_landing_meanweight_curryear <- as.numeric(as.character( Interactionsyear[[currenttime]][[j]]@interactions[[int_ass]]$landings@meanWeight.CI.perc[1,PERC] )) 
   
if (n_fleet_for_species > 1) {
landing_weight <- production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],(4 + 3 * n_fleet_for_species + 3):(4 + 4* n_fleet_for_species + 3)]
landing_mean_length <- production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],(4 + 4 * n_fleet_for_species + 4):(4 + 5* n_fleet_for_species + 4)]
} else {
landing_weight <- data.frame(cbind(production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],7] , production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],7]))
colnames(landing_weight) <- c(colnames(production_table_all_runs)[7], "X")
landing_mean_length <- data.frame(cbind(production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],8] , production_table_all_runs[production_table_all_runs$Year == c(years, years.forecast)[currenttime],8]))
colnames(landing_mean_length) <- c(colnames(production_table_all_runs)[8], "X")
}

      lw_a_M <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[1,1])
      lw_b_M <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[1,2])
      lw_a_F <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[2,1])
      lw_b_F <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[2,2])
      lw_a <- mean(c(lw_a_M, lw_a_F))
      lw_b <- mean(c(lw_b_M, lw_b_F))
 
   bmtprice_landing_meanweight_curryear <-  lw_a * as.numeric(landing_mean_length[,int_ass+1])^ lw_b #production_table_all_runs
      print( "Mean weight for the 500 runs")
       print(bmtprice_landing_meanweight_curryear)
            
#       Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc[1,PERC] <- as.numeric(landing_weight[yy,fleet_interaction_ord+1] )
#        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc[1,PERC] <- as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])
#Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc[1,PERC] <-  lw_a * as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])^ lw_b
#   
#   
   
#bmtprice_landing_meanweight_curryear <- as.numeric(as.character( Interactionsyear[[currenttime]][[j]]@interactions[[int_ass]]$landings@meanWeight.CI.perc[1,PERC] )) / 1000   # modifica fatta il 29/12/2017
 
# bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC, j] ))       # modifica fatta il 29/12/2017
# bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC, j] )) /1000
    
    bmtprice_landing_weight_curryear <-  as.numeric(landing_weight[,int_ass+1] )
     print( "Landing weight for the 500 runs")
            print(bmtprice_landing_weight_curryear)
             
      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))
      bmtprice_import_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@import.weight[j] ))
      bmtprice_elastity_coeff_COL2 <-  as.numeric(as.character(pmat2[j,i]))
          # if (landing_catch_COL4 == 1) {
#      bmtprice_landing_number_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.number[j] ))
#      } else {
#            bmtprice_landing_number_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.number[j] ))  +  as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@discard.number[j] ))
#      }
      bmtprice_elastity_coeff_COL3 <-  as.numeric(as.character(pmat3[j,i]))
     
          if (bmtprice_import_weight_curryear != -1) {       

N_RUNS <- bmtprice_lastSim_year * ( bmtprice_landing_weight_curryear ^ bmtprice_elastity_coeff_COL1)*((bmtprice_import_weight_curryear) ^ bmtprice_elastity_coeff_COL2)*( bmtprice_landing_meanweight_curryear ^ bmtprice_elastity_coeff_COL3 )

} else {
N_RUNS <- bmtprice_lastSim_year * ( bmtprice_landing_weight_curryear ^ bmtprice_elastity_coeff_COL1)*( bmtprice_landing_meanweight_curryear ^ bmtprice_elastity_coeff_COL3)
}		                                  
		# }	

Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[, j] <-   quantile(as.numeric(as.character(N_RUNS)), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )

			}
    }
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 3   
# --------------------------------------------------------------------------------------------------------

  } else if (option[[1]] == 3) { # Fishrent
    for (i in 1:n_fleet) {
      for (j in 1:m_stock) {
      
      bmtprice_prevyear <- as.numeric(as.character(Flyear[[currenttime - 1]]@fleetsegments[[i]]@price[j] ))
      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))

            bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight[j] ))
            bmtprice_landing_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@landing.weight[j] ))
     
Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_prevyear * (bmtprice_landing_weight_curryear / bmtprice_landing_weight_prevyear)  

      
      for (PERC in c(1:5)) {	      
      bmtprice_prevyear <- as.numeric(as.character(Flyear[[currenttime - 1]]@fleetsegments[[i]]@price.CI.perc[PERC, j] ))
      bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))

            bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j] ))
            bmtprice_landing_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j] ))
      
Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[PERC, j] <- bmtprice_prevyear * ( bmtprice_landing_weight_curryear / bmtprice_landing_weight_prevyear) 


			}
      }
    } 
  
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 4   
# --------------------------------------------------------------------------------------------------------  
  } else if (option[[1]] == 4) { # Bemmfish
  
    for (j in 1:m_stock) {
      for (i in 1:n_fleet) {

        
bmtprice_lastSim_year <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price[j] )) 
#print(paste("Peso medio",bmtprice_landing_meanweight_curryear))
bmtprice_landing_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight[j] ))

bmtprice_elastity_coeff_COL1 <- as.numeric(as.character( pmat1[j,i] ))
            
Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_lastSim_year * e ^ ( bmtprice_landing_weight_curryear * bmtprice_elastity_coeff_COL1)
      
			for (PERC in c(1:5)) {	   
     Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[PERC, j] <- bmtprice_lastSim_year * e ^ ( bmtprice_landing_weight_curryear * bmtprice_elastity_coeff_COL1)
			
			}

			}
    }

        
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 5   
# --------------------------------------------------------------------------------------------------------    
    
  } else if (option[[1]]==5) { #  prices of last year
    for (i in 1:n_fleet) {
      for (j in 1:m_stock) {
      
      bmtprice_price_landing_prevyear <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price[j] ))
        Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- bmtprice_price_landing_prevyear
        
        				for (PERC in c(1:5)) {
        		      bmtprice_price_landing_prevyear <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price.CI.perc[PERC,j] ))
        Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[PERC,j] <- bmtprice_price_landing_prevyear		
        				}
        
      }
    }
  
  
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 6   
# --------------------------------------------------------------------------------------------------------    
    
  } else if (option[[1]]==6) { #  costant price
  
         for (i in 1:n_fleet) {
      for (j in 1:m_stock) {

          price_land_COL1 <- as.numeric(as.character( pmat1[j,i] ))

Flyear[[currenttime]]@fleetsegments[[i]]@price[j] <- price_disc_COL4
      
      for (PERC in c(1:5)) {

          price_land_COL1 <- as.numeric(as.character( pmat1[j,i] ))
          
      Flyear[[currenttime]]@fleetsegments[[i]]@price.CI.perc[PERC,j] <- price_land_COL1

      

			}
      }
    }
  
  }
  
  return(Flyear)
}
