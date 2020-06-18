# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# import of landings in weight and number by fleet segment from biological module

updateFleetfromInteraction <- function(Fyear, year_to_be_updated) {

if (FALSE) {
    Fyear = Fleetyear
    year_to_be_updated =  1 #TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR
    fleet_interaction_ord=1
}

if (phase == "SIMULATION") {
    years_tobeup <- years
    year_index <- year_to_be_updated
} else {
    years_tobeup <- years.forecast
    year_index <- year_to_be_updated - simperiod 
}
      Landingsfleet <- data.frame(matrix(0, nrow=1, ncol=n_fleet))
      Landingsfleet_perc <- data.frame(matrix(0, nrow=5, ncol=n_fleet))
      Discardsfleet <- data.frame(matrix(0, nrow=1, ncol=n_fleet))
      Discardsfleet_perc <- data.frame(matrix(0, nrow=5, ncol=n_fleet))

 print(paste("Updating Fleet from Interaction objects for", years_tobeup[year_index],"..."), quote=F)   
 
   # for (i in 1:n_fleet) {
      for (j in 1:length(BMT_SPECIES)) {
      
       SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", j, ".StockAssessmentTool", sep=""),1])
      ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", j, ".AladymSimulation", sep=""),1])

     # print(SAtool)

     if (phase == "SIMULATION") {
      condition <- !(SAtool != "XSA" & !ALADYM_flag)
  } else {
      condition <- BMT_SCENARIO != BMT_HR_CHANGE_TOTAL_FISHMORTALITY
  }
  
        # print(paste("specie", BMT_SPECIES[j]))  
associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", j, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
  
   if (condition) {
   
  # print("dall'oggetto fleetstock")

      fleet_interaction_ord <- 1
      for (i in 1:length(BMT_FLEETSEGMENTS))  {
          if (i %in% associated_fleetsegment_indices) {
          
#Interactionsyear[[yy]][[j]]@interactions[[fleet_interaction_ord]]$landings

      # print(paste("flotta", BMT_FLEETSEGMENTS[i]))  
    Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.number[j] <-  sum( as.numeric(as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@numbers)) )
        # -- added in the new version
    Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.number[j] <-  sum( as.numeric(as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$discards@numbers)) )  
        # take the number and report it in the summary table from corresponding time t of Interaction objects 
    
    # fixed on 27.01.2015
    if (phase=="FORECAST") {
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j] <-  Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@totalweight * 1000
     }
 # -------------------------------------------------
				 # -- added in the new version
         tot_weigh <- Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$discards@totalweight
    Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight[j] <-  ifelse(tot_weigh == -1, 0, tot_weigh*1000)      
        # take the number and report it in the summary table from corresponding time t of Interaction objects   
				   
        Landingsfleet[i] <- sum(Landingsfleet[i],Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j], na.rm=TRUE)    # landings target species
         Discardsfleet[i] <- sum(Discardsfleet[i], as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight[j])), na.rm=TRUE)    # landings target species
# 
# if (phase != "SIMULATION") {
# 
# equation_type_COL1 <- as.numeric(as.character(eimat[1,i]))
#
#	 if (equation_type_COL1 == 1) {
#	 # read col 2 and 3
#	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
#	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))
#
# Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- correction_factor_landing_COL2 * as.numeric(as.character(Landingsfleet[i]))   # total landings
#
#	  } else {
#	
#	 coeff_factor_landing_v_COL4 <- as.numeric(as.character(eimat[4,i]))
#	  coeff_factor_landing_u_COL5 <- as.numeric(as.character(eimat[5,i]))
#	
#if (equation_type_COL1 == 2) {
# # option 2 : additive (u + v*sumL)
#	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- correction_factor_u_COL2 + correction_factor_v_COL5 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T) 
#
#} else {
# # option 3 : multiplicative u*(sumL)^v
#	 	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- correction_factor_u_COL2 * (  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T) ^correction_factor_v_COL5 ) 
#}	
# 
#
#}
#}
#
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing_obligation[j] <- 	as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landing_obligation ) 
   
	# --------------------------------------
		# CI
		# --------------------------------------
 if (dim(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc)[1] != 0)  {
for (PERC in c(1:5)) {

 if (dim(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@numbers.CI.perc)[1] != 0)  {
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.number.CI.perc[PERC,j] <-  sum( as.numeric(as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@numbers.CI.perc[PERC,])) )

}

 if (dim(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$discards@numbers.CI.perc)[1] != 0)  {
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.number.CI.perc[PERC,j] <-  sum( as.numeric(as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$discards@numbers.CI.perc[PERC,])) )  
}
# take the number and report it in the summary table from corresponding time t of Interaction objects 
 
   # fixed on 27.01.2015
    if (phase=="FORECAST") {   
Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,j] <-  Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc[1,PERC] * 1000
      }
      
   	 # -- added in the new version
         tot_weigh <- Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc[1,PERC]
    Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC, j] <-  ifelse(tot_weigh == -1, 0, tot_weigh*1000)      
        # take the number and report it in the summary table from corresponding time t of Interaction objects      

Landingsfleet_perc[PERC, i] <- sum(Landingsfleet_perc[PERC, i], Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC, j], na.rm=TRUE) 
Discardsfleet_perc[PERC, i] <- sum(Discardsfleet_perc[PERC, i], as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC, j])), na.rm=TRUE) 

# if (phase != "SIMULATION") {         
# Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- as.numeric(as.character(eimat[1,i])) * as.numeric(as.character(Landingsfleet_perc[PERC,i]))   # total landings
# } else {
#  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings))   # total landings
# }
# 
# 
 
#  if (phase != "SIMULATION") {
# 
# equation_type_COL1 <- as.numeric(as.character(eimat[1,i]))
#
#	 if (equation_type_COL1 == 1) {
#	 # read col 2 and 3
#	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
#	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))
#
# Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- correction_factor_landing_COL2 * as.numeric(as.character(Landingsfleet_perc[PERC,i]))   # total landings
#
#	  } else {
#	
#	 coeff_factor_landing_v_COL4 <- as.numeric(as.character(eimat[4,i]))
#	  coeff_factor_landing_u_COL5 <- as.numeric(as.character(eimat[5,i]))
#	
#if (equation_type_COL1 == 2) {
#
# # option 2 : additive (u + v*C)
#	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- sum( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])), na.rm=T) , (correction_factor_u_COL2 + correction_factor_v_COL5 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)  ), na.rm=T)
#
#} else {
#
# # option 3 : multiplicative (u*v*C)
#	 	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- sum( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])), na.rm=T) , (correction_factor_u_COL2  * ( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)  ^ correction_factor_v_COL5 ) ), na.rm=T)
#}	
#
#}
#} else {
#
#  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings))   # total landings
# 
# }
 

}    # end percentiles

}

       # print(data.frame(Fleetyear[[yy]]@fleetsegments[[i]]@landing.weight)[1,j])
       fleet_interaction_ord <- fleet_interaction_ord + 1
      } else {
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j] <- 0
      }
   }
     
   
# _______________________________________________________________________________________________________    
    
    
    } else {
     #   print("da historical landings")
        
              fleet_interaction_ord <- 1
        for (i in 1:length(BMT_FLEETSEGMENTS))  {
          if (i %in% associated_fleetsegment_indices) {

      if (SAtool != "XSA" & !ALADYM_flag) {
               Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j] <-  sum(as.numeric(as.character(Interactionsyear[[year_to_be_updated]][[j]]@interactions[[fleet_interaction_ord]]$historicalLandings)) )
      }
             Landingsfleet[i] <- sum(Landingsfleet[i],Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j],na.rm=TRUE)    # landings target species  
       
 Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- as.numeric(as.character(eimat[1,i])) * as.numeric(as.character(Landingsfleet[i]))   # total landings
             fleet_interaction_ord <- fleet_interaction_ord + 1      
            } else {
      Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight[j] <- 0
      }
  
# inserire il controllo su SAtool

} # end fleet segment

} # end condition


} # end species

#
#flottaN <- 1
#Interactionsyear[[1]][[2]]@interactions[[flottaN]]$landings
# flottaN_caso <- 1
#Fleetyear[[1]]@fleetsegments[[flottaN_caso]]@landing.weight
#Fleetyear[[1]]@fleetsegments[[flottaN_caso]]@landing.number


  for (i in 1:length(BMT_FLEETSEGMENTS))  {
  
  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.discards <- sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight)) , na.rm=T)

 if (phase != "SIMULATION") {
 
 equation_type_COL1 <- as.numeric(as.character(eimat[1,i]))

	 if (equation_type_COL1 == 1) {
	 # read col 2 and 3
	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))

 Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- correction_factor_landing_COL2 * as.numeric(as.character(Landingsfleet[i]))   # total landings

	  } else {
	
	 coeff_factor_landing_u_COL4 <- as.numeric(as.character(eimat[4,i]))
	  coeff_factor_landing_v_COL5 <- as.numeric(as.character(eimat[5,i]))
	
if (equation_type_COL1 == 2) {
 # option 2 : additive (u + v*sumL)
	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- sum(coeff_factor_landing_u_COL4 , coeff_factor_landing_v_COL5 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T), sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T), na.rm=T) 

} else {
 # option 3 : multiplicative u*(sumL)^v
	 	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- sum(sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T), coeff_factor_landing_u_COL4 * (  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T) ^coeff_factor_landing_v_COL5 )  , na.rm=T)
}	
 

}
}

 
 	# --------------------------------------
		# CI
		# --------------------------------------

for (PERC in c(1:5)) {

  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.discards.CI.perc[1, PERC] <-  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC,])) , na.rm=T)

  if (phase != "SIMULATION") {
 
 equation_type_COL1 <- as.numeric(as.character(eimat[1,i]))

	 if (equation_type_COL1 == 1) {
	 # read col 2 and 3
	 correction_factor_landing_COL2 <- as.numeric(as.character(eimat[2,i]))
	  correction_factor_revenues_COL3 <- as.numeric(as.character(eimat[3,i]))

 Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- correction_factor_landing_COL2 * as.numeric(as.character(Landingsfleet_perc[PERC,i]))   # total landings

	  } else {
	
	 coeff_factor_landing_u_COL4 <- as.numeric(as.character(eimat[4,i]))
	  coeff_factor_landing_v_COL5 <- as.numeric(as.character(eimat[5,i]))
	
if (equation_type_COL1 == 2) {

#  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings <- sum(coeff_factor_landing_u_COL4 , coeff_factor_landing_v_COL5 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T), sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight)) , na.rm=T), na.rm=T) 


 # option 2 : additive (u + v*C)
	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- sum( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])), na.rm=T) , (coeff_factor_landing_u_COL4 + coeff_factor_landing_v_COL5 *  sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)  ), na.rm=T)

} else {

 # option 3 : multiplicative (u*v*C)
	 	  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- sum( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])), na.rm=T) , (coeff_factor_landing_u_COL4  * ( sum(as.numeric(as.character( Fyear[[year_to_be_updated]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,])) , na.rm=T)  ^ coeff_factor_landing_v_COL5 ) ), na.rm=T)
}	

}
} else {

  Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings.CI.perc[1, PERC] <- as.numeric(as.character(Fyear[[year_to_be_updated]]@fleetsegments[[i]]@total.landings))   # total landings
 
 }
 

}    # end percentiles
 
 
  
  
  }   
  
  
  
  
  
return(Fyear)

}