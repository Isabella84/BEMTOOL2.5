# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTFlbeh.R - Bemtool fleet behaviour component
# Author: Paolo Accadia

BMTFlbeh <- function(option, Flyear, fdmat, eimat, admat, tpmat, year_to_change, simperiod, n_fleet) {

if (exists("to_be_updated")) { 
rm(to_be_updated) 
}

 for (num_fl in 1:n_fleet) {
 present <- FALSE
  for (speci in 1:length(BMT_SPECIES)){
      if (!present) {
            associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", speci, ".associatedFleetsegment", sep=""), ])   
            associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
            associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
            if (num_fl %in% associated_fleetsegment_indices) {
                present <- TRUE
                if (!exists("to_be_updated")) {
                      to_be_updated <- num_fl
                } else {
                      to_be_updated <- c(to_be_updated, num_fl)
                }
            }    
      }
  }
}

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 Fleet dynamics
  if (option[6] == 1) { #        Fishrent    (unique option)
    for (i in to_be_updated) {
    
    bmtfleetdyn_low_invest_COL1 <-  as.numeric(as.character(fdmat[1,i]))
    bmtfleetdyn_upp_invest_COL2 <-  as.numeric(as.character(fdmat[2,i]))
    bmtfleetdyn_share_profit_COL3 <- as.numeric(as.character(fdmat[3,i]))
                                                               # bmtfleetdyn_prev_total_revenues = as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@total.revenues.CI.perc[3]  ))  
   # bmtfleetdyn_prev_total_revenues  <- as.numeric(as.character(Flyear[[year_to_change-1]]@fleetsegments[[i]]@total.revenues ))
    bmtfleetdyn_prev_total_revenues <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@total.revenues.CI.perc[3]  ))     
                                                               #  bmtfleetdyn_prev_vessels = as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@VESSELS.annual ))
    bmtfleetdyn_prev_vessels <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@VESSELS.annual ))
                                                                # bmtfleetdyn_prev_BER = as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@EC.BER.CI.perc[3]  ))
   # bmtfleetdyn_prev_BER <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@EC.BER ))        
    bmtfleetdyn_prev_BER <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@EC.BER.CI.perc[3]  ))       
                                                             #    bmtfleetdyn_curr_invest_number = as.numeric(as.character( Fleetyear[[59]]@fleetsegments[[1]]@investment$number ))
   # bmtfleetdyn_curr_invest_number <- as.numeric(as.character( Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number ))
    bmtfleetdyn_curr_invest_number <- as.numeric(as.character( Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number ))
  
#    cond2 <- ifelse(bmtfleetdyn_prev_total_revenues == 0, 0, bmtfleetdyn_share_profit_COL3 * ( bmtfleetdyn_prev_total_revenues - bmtfleetdyn_prev_BER ) / bmtfleetdyn_prev_total_revenues )
    
    cond2 <- ifelse(bmtfleetdyn_prev_total_revenues == 0, 0, bmtfleetdyn_share_profit_COL3 * ( bmtfleetdyn_prev_total_revenues - bmtfleetdyn_prev_BER ) / bmtfleetdyn_prev_total_revenues )
   
      if (bmtfleetdyn_prev_BER < 0) {
             #print("bmtfleetdyn_prev_BER < 0")
              # investment in number
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number <- - bmtfleetdyn_low_invest_COL1 * bmtfleetdyn_prev_vessels
      
      } else if (cond2 > bmtfleetdyn_upp_invest_COL2 ) {
        # print("cond2 > bmtfleetdyn_upp_invest_COL2")
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number <- bmtfleetdyn_upp_invest_COL2 * bmtfleetdyn_prev_vessels
      
      } else if (cond2 < - bmtfleetdyn_low_invest_COL1 ) {
          # print("cond2 < - bmtfleetdyn_low_invest_COL1")
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number <- - bmtfleetdyn_low_invest_COL1 * bmtfleetdyn_prev_vessels
      
      } else {
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number <- ( bmtfleetdyn_share_profit_COL3 * (bmtfleetdyn_prev_total_revenues - bmtfleetdyn_prev_BER ) / bmtfleetdyn_prev_total_revenues ) * bmtfleetdyn_prev_vessels
     
      }
      
       # investment in value from the number
    Flyear[[year_to_change]]@fleetsegments[[i]]@investment$value <- bmtfleetdyn_share_profit_COL3 * bmtfleetdyn_curr_invest_number
    
    }
  } else  { # no investment
  print("No investement!")
    for (i in to_be_updated) {
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$number <- 0 
      Flyear[[year_to_change]]@fleetsegments[[i]]@investment$value <- 0
     }
  }
  

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 Activity dynaimcs
  if (option[7] == 1) { # Fishrent   
           
    for (i in to_be_updated) {
    
    bmtfleetact_low_COL1 <-  as.numeric(as.character(admat[1,i]))
    bmtfleetact_upp_COL2 <-  as.numeric(as.character(admat[2,i]))
    bmtfleetact_profitability_COL3 <- as.numeric(as.character(admat[3,i]))
                                                                            
#   bmtfleetact_prev_total_revenues <- as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@total.revenues.CI.perc[3]  ))
#    bmtfleetact_prev_BER <- as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@EC.BER.CI.perc[3]  ))        
#    bmtfleetact_prev_average_days <- as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@DAYS.average.annual ))
#    bmtfleetact_prev_MAX_average_days <- as.numeric(as.character( Fleetyear[[58]]@fleetsegments[[1]]@MAXDAYS.average.annual  ))                                                                    
                                                                            
    bmtfleetact_prev_total_revenues <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@total.revenues.CI.perc[3]  ))
    bmtfleetact_prev_BER <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@EC.BER.CI.perc[3]  ))        
    bmtfleetact_prev_average_days <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@DAYS.average.annual ))
    bmtfleetact_prev_MAX_average_days <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@MAXDAYS.average.annual  ))
    
    cond2 <- ifelse( bmtfleetact_prev_total_revenues == 0, 0, bmtfleetact_profitability_COL3 * ( bmtfleetact_prev_total_revenues - bmtfleetact_prev_BER ) / bmtfleetact_prev_total_revenues )
    
      if (bmtfleetact_prev_BER < 0) {
      
      # current annual average days at sea
      Flyear[[year_to_change]]@fleetsegments[[i]]@DAYS.average.annual <- ( 1 - bmtfleetact_low_COL1 ) * bmtfleetact_prev_average_days
      
      } else if ( cond2 > bmtfleetact_upp_COL2 ) {
      
      Flyear[[year_to_change]]@fleetsegments[[i]]@DAYS.average.annual <- min( (1 + bmtfleetact_upp_COL2 ) * bmtfleetact_prev_average_days , bmtfleetact_prev_MAX_average_days )
      
      } else if (cond2 < - bmtfleetact_low_COL1 ) {
      
      Flyear[[year_to_change]]@fleetsegments[[i]]@DAYS.average.annual <- (1 - bmtfleetact_low_COL1 ) * bmtfleetact_prev_average_days
      
      } else {
      
      Flyear[[year_to_change]]@fleetsegments[[i]]@DAYS.average.annual <- min( ( 1 + ( bmtfleetact_profitability_COL3 * ( bmtfleetact_prev_total_revenues - bmtfleetact_prev_BER )/ bmtfleetact_prev_total_revenues ))* bmtfleetact_prev_average_days, bmtfleetact_prev_MAX_average_days )
      
      }
    }      
#Flyear[[year_to_change]]@fleetsegments[[i]]@average.daysatsea[1] <- min(Flyear[[year_to_change]]@fleetsegments[[i]]@average.daysatsea[2],Flyear[[year_to_change]]@fleetsegments[[i]]@average.daysatsea[3])
  } else { # no change in days at sea
    for (i in to_be_updated) {
        bmtfleetact_prev_average_days <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@DAYS.average.annual ))
    
      Flyear[[year_to_change]]@fleetsegments[[i]]@DAYS.average.annual <- bmtfleetact_prev_average_days 
    }
  }  
  
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 Technological progress
  if (option[8] == 1) { # Mefisto
    for (i in to_be_updated) {
    
     bmttechprogress_initial <- as.numeric(as.character(tpmat[1,i]))
      bmttechprogress_fraction_time <- as.numeric(as.character(tpmat[2,i]))
      bmttechprogress_invested_capital <- as.numeric(as.character(tpmat[3,i]))
      bmttechprogress_prev_capital <- as.numeric(as.character( Flyear[[year_to_change-1]]@fleetsegments[[i]]@capital.value ))
      bmttechprogress_firstyear_capital <- as.numeric(as.character( Flyear[[1]]@fleetsegments[[i]]@capital.value ))
      
      Flyear[[year_to_change]]@fleetsegments[[i]]@technology <- bmttechprogress_initial * (bmttechprogress_fraction_time ^ (year_to_change-simperiod) ) * ( (1-exp(-bmttechprogress_invested_capital * bmttechprogress_prev_capital )) / (1-exp(-bmttechprogress_invested_capital * bmttechprogress_firstyear_capital )))
    }
  } else { # no change
    for (i in to_be_updated) {
           
      bmttechprogress_firstyear <- as.numeric(as.character( Flyear[[1]]@fleetsegments[[i]]@technology ))
            
      Flyear[[year_to_change]]@fleetsegments[[i]]@technology <- bmttechprogress_firstyear
    }
  }
  
  return(Flyear)
  }
