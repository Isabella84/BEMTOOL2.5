# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

updateBiologicalfromALADYM<- function(ALADYM_spe, Populs, Inters, Fleet, ALADYM_rp_calc) {

 if (FALSE) {
Populs = Populations
Inters = Interactionsyear
Fleet = Fleetyear
ALADYM_rp_calc = ALADYM_reference_points_calc
}
# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE POPULATION in aladym
# ----------------------------------------------------------------------------

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

      lw_a_M <- as.numeric(Populs[[ALADYM_spe]]@lengthweight[1,1])
      lw_b_M <- as.numeric(Populs[[ALADYM_spe]]@lengthweight[1,2])
      lw_a_F <- as.numeric(Populs[[ALADYM_spe]]@lengthweight[2,1])
      lw_b_F <- as.numeric(Populs[[ALADYM_spe]]@lengthweight[2,2])
      lw_a <- mean(c(lw_a_M, lw_a_F))
      lw_b <- mean(c(lw_b_M, lw_b_F))
# fertility rate
Populs[[ALADYM_spe]]@offspring.prop[1,]  <- c(INP$Fertility_Rate)

   SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1])
   
# if (SAtool == "NONE") {

n_ages_mal <- INP$MGrowth_tend
n_ages_fem <- INP$FGrowth_tend

first_age <- 0

    n_ages_mal <- n_ages_mal - trunc(INP$tr/12)
        n_ages_fem <- n_ages_fem - trunc(INP$tr/12)
    first_age <- trunc(INP$tr/12)
#} 

if (modulo(INP$tr,12) !=0 | INP$tr==0) { 
for (ag in 1:INP$MGrowth_tend) {

  if ((ag-1) < first_age) {    # tr > di 1 anno
   Populs[[ALADYM_spe]]@M.vect$M[ag,] <- rep(NA, 12)
   Populs[[ALADYM_spe]]@maturity.vect[1,ag] <- 0
  # print(paste("eta #", ag,"tutti NA"))
      } else {
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla              
           start_mon_age <-  1 
           if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
          # print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           mort_vect_this_age <- c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) , BAS$MM[start_mon_age:end_mon_age])
           Populs[[ALADYM_spe]]@maturity.vect[1,ag] <- mean(as.numeric(as.character(BAS$MMaturity[start_mon_age:end_mon_age])) )    
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
           Populs[[ALADYM_spe]]@maturity.vect[1,ag] <- mean(as.numeric(as.character(BAS$MMaturity[start_mon_age:end_mon_age])) )   
        } 
        
 Populs[[ALADYM_spe]]@M.vect$M[ag,(12-length(mort_vect_this_age)+1):12]  <- mort_vect_this_age #[(length(mort_vect_this_age)-11):length(mort_vect_this_age)]     

      }
}

} else {

for (ag in 1:INP$MGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
   Populs[[ALADYM_spe]]@M.vect$M[ag,] <- c(rep(NA, 11) , mort_vect_this_age)
   Populs[[ALADYM_spe]]@maturity.vect[1,ag] <- mean(as.numeric(as.character(BAS$MMaturity[start_mon_age:end_mon_age])) )    
  # print(paste("eta #", ag,"tutti NA"))
      } else {
              start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
         Populs[[ALADYM_spe]]@M.vect$M[ag,]  <- mort_vect_this_age
         Populs[[ALADYM_spe]]@maturity.vect[1,ag] <- mean(as.numeric(as.character(BAS$MMaturity[start_mon_age:end_mon_age])) )      
} 

}                                

}

# FEMALES

if (modulo(INP$tr,12) !=0 | INP$tr==0) {
# natural mortality females
for (ag in 1:INP$FGrowth_tend) {

  if ((ag-1) < first_age ) {
   Populs[[ALADYM_spe]]@M.vect$F[ag,] <- rep(NA, 12)
   Populs[[ALADYM_spe]]@maturity.vect[2,ag] <- 0   
  # print(paste("eta #", ag,"tutti NA"))
      } else {      
      
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla              
            start_mon_age <-  1 
 if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
        #   print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           mort_vect_this_age <-  c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) ,BAS$FM[start_mon_age:end_mon_age])
           Populs[[ALADYM_spe]]@maturity.vect[2,ag] <- mean(as.numeric(as.character(BAS$FMaturity[start_mon_age:end_mon_age])) )    
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
        #   print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
           Populs[[ALADYM_spe]]@maturity.vect[2,ag] <- mean(as.numeric(as.character(BAS$FMaturity[start_mon_age:end_mon_age])) )    
        } 
#
#      if (INP$tr==0) {
#         mort_vect_this_age <- mort_vect_this_age[(length(mort_vect_this_age)-12+1):length(mort_vect_this_age)]
#      }
 Populs[[ALADYM_spe]]@M.vect$F[ag,(12-length(mort_vect_this_age)+1):12]  <- mort_vect_this_age #[(length(mort_vect_this_age)-11):length(mort_vect_this_age)]     
      }
} 

} else {






for (ag in 1:INP$FGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
   Populs[[ALADYM_spe]]@M.vect$F[ag,] <- c(rep(NA, 11) , mort_vect_this_age)
   Populs[[ALADYM_spe]]@maturity.vect[2,ag] <- mean(as.numeric(as.character(BAS$FMaturity[start_mon_age:end_mon_age])) )    
  # print(paste("eta #", ag,"tutti NA"))


      } else {
              start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
         Populs[[ALADYM_spe]]@M.vect$F[ag,]  <- mort_vect_this_age
         Populs[[ALADYM_spe]]@maturity.vect[2,ag] <- mean(as.numeric(as.character(BAS$FMaturity[start_mon_age:end_mon_age])) )    
} 

}                                



} 

















# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE CATCHES (Landings, Discards and Catches) in aladym
# ----------------------------------------------------------------------------

source(paste(ALADYM_home, "/src/paths.r", sep=""))


# catch, landing and discard by age 
catch_by_age <- read.csv(CATCHBYAGE_table, sep=";")
landing_by_age <- read.csv(LANDINGBYAGE_table, sep=";")
discard_by_age <- read.csv(DISCARDBYAGE_table, sep=";")

n_fleet_for_species <- length(associated_fleetsegment_indices )
#catch_by_age_F <- catch_by_age[1:(simperiod*n_fleet_for_species),] 
#catch_by_age_M <- catch_by_age[( (simperiod*n_fleet_for_species) +1):nrow(catch_by_age),] 
#landing_by_age_F <- landing_by_age[1:(simperiod*n_fleet_for_species),] 
#landing_by_age_M <- landing_by_age[( (simperiod*n_fleet_for_species) +1):nrow(landing_by_age),] 
#discard_by_age_F <- discard_by_age[1:(simperiod*n_fleet_for_species),] 
#discard_by_age_M <- discard_by_age[( (simperiod*n_fleet_for_species) +1):nrow(discard_by_age),] 

catch_by_age_F <- catch_by_age[as.character(catch_by_age[,(ncol(catch_by_age)-1)] ) == "F",]      
catch_by_age_M <- catch_by_age[as.character(catch_by_age[,(ncol(catch_by_age)-1)] ) == "M",]  #catch_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(catch_by_age),] 
landing_by_age_F <- landing_by_age[as.character(landing_by_age[,(ncol(landing_by_age)-1)] ) == "F",]   #landing_by_age[1:((bmt_years_forecast+simperiod)*n_fleet_for_species),] 
landing_by_age_M <- landing_by_age[as.character(landing_by_age[,(ncol(landing_by_age)-1)] ) == "M",]  #landing_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(landing_by_age),] 
discard_by_age_F <- discard_by_age[as.character(discard_by_age[,(ncol(discard_by_age)-1)] ) == "F",]   #discard_by_age[1:((bmt_years_forecast+simperiod)*n_fleet_for_species),] 
discard_by_age_M <- discard_by_age[as.character(discard_by_age[,(ncol(discard_by_age)-1)] ) == "M",]  #discard_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(discard_by_age),] 

  n_ages <- max(as.numeric(new_aldPopulation@lifespan[1,1]) , as.numeric(new_aldPopulation@lifespan[2,1]) )     
  first_age <- 0
  n_ages <- n_ages - trunc(INP$tr/12)
  first_age <- trunc(INP$tr/12)
  vector_ages <- c(first_age:(n_ages+first_age-1))

catch_total <- cbind(catch_by_age_F[,1], cbind(catch_by_age_F[,2:(length(vector_ages)+1)]  +  catch_by_age_M[,2:(length(vector_ages)+1)], catch_by_age_F[,ncol(catch_by_age_F)]))
landing_total <- cbind(landing_by_age_F[,1], cbind(landing_by_age_F[,2:(length(vector_ages)+1)]  +  landing_by_age_M[,2:(length(vector_ages)+1)], landing_by_age_F[,ncol(landing_by_age_F)]))
discard_total <- cbind(discard_by_age_F[,1], cbind(discard_by_age_F[,2:(length(vector_ages)+1)]  +  discard_by_age_M[,2:(length(vector_ages)+1)], discard_by_age_F[,ncol(discard_by_age_F)]))

colnames(catch_total) <- c(colnames(catch_by_age_F)[1:(ncol(catch_by_age_F)-2)], colnames(catch_by_age_F)[ncol(catch_by_age_F)])
colnames(landing_total) <- c(colnames(landing_by_age_F)[1:(ncol(landing_by_age_F)-2)], colnames(landing_by_age_F)[ncol(landing_by_age_F)])
colnames(discard_total) <- c(colnames(discard_by_age_F)[1:(ncol(discard_by_age_F)-2)], colnames(discard_by_age_F)[ncol(discard_by_age_F)])

percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)
percs_table <-	data.frame(matrix(0, nrow=5, ncol=length( vector_ages)))
colnames(percs_table) <- c(paste("age", vector_ages, sep=""))
rownames(percs_table) <- percentiles_numb 
percs_table_one_row <-	data.frame(matrix(0, nrow=1, ncol=5))
colnames(percs_table_one_row) <- percentiles_numb

   for (yy in 1:simperiod) {

#      TOTAL_LANDING_numbers <-  data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
#      rownames(TOTAL_LANDING_numbers) <- c(years[yy])
#      colnames(TOTAL_LANDING_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
#     
#      TOTAL_CATCH_numbers <- data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
#      rownames(TOTAL_CATCH_numbers) <- c(years[yy])
#      colnames(TOTAL_CATCH_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
#  
#        TOTAL_DISCARD_numbers  <- data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
#      rownames(TOTAL_DISCARD_numbers) <- c(years[yy])
#      colnames(TOTAL_DISCARD_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
#     
     # TOTAL_DISCARD_numbers_NA <- data.frame(matrix(NA, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
#      rownames(TOTAL_DISCARD_numbers_NA) <- c(years[yy])
#      colnames(TOTAL_DISCARD_numbers_NA) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  


 TOTAL_matrix <- data.frame(matrix(0, nrow=1, ncol=length(vector_ages)))
  rownames(TOTAL_matrix) <- c(years[yy])
  colnames(TOTAL_matrix) <- c(paste("age", vector_ages, sep=""))  
        # [,which(colnames(Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers) %in% paste("age", c(first_age:n_ages_mal), sep=""))]
      Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers <- TOTAL_matrix
			Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers.CI.perc <- percs_table

			Inters[[yy]][[ALADYM_spe]]@totallanding@numbers <- TOTAL_matrix
			Inters[[yy]][[ALADYM_spe]]@totallanding@numbers.CI.perc <- percs_table

			Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers <- TOTAL_matrix
			Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers.CI.perc <- percs_table
 
     fleet_interaction_ord <- 1 
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {

if (n_fl %in% associated_fleetsegment_indices)  {

  catch_fleet <- catch_total[ (1+(fleet_interaction_ord-1)*simperiod) : (fleet_interaction_ord*simperiod),]
    landing_fleet <- landing_total[ (1+(fleet_interaction_ord-1)*simperiod) : (fleet_interaction_ord*simperiod),]
      discard_fleet <- discard_total[ (1+(fleet_interaction_ord-1)*simperiod) : (fleet_interaction_ord*simperiod),]

# ------------------------------------------------------------------- CATCHES

numbers_df <- data.frame(matrix(0, nrow=1, ncol=length(vector_ages)))
rownames(numbers_df) <- c(years[yy])
colnames(numbers_df) <- c(paste("age", vector_ages, sep=""))  
numbers_df[1, which(colnames(numbers_df) %in% paste("age", c(first_age:n_ages_mal), sep=""))] <- catch_fleet[yy,2:(ncol(catch_fleet)-1)]                       # modificare quiiiiiiiiiiiiiiiiiiii ( l'indice della matrice numbers: selezione colonne in base a tr)

Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers <- numbers_df 

percs_table <-	data.frame(matrix(0, nrow=5, ncol=length(vector_ages)))
colnames(percs_table) <- c(paste("age", vector_ages, sep="")) 
rownames(percs_table) <- percentiles_numb 


Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers.CI.perc <- percs_table


for (PERC in 1:length(percentiles_numb)) {


Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers.CI.perc[PERC,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers



}	
		
Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers <- Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers + Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers 
Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers.CI.perc[,] <- Inters[[yy]][[ALADYM_spe]]@totalcatch@numbers

# ------------------------------------------------------------------- LANDINGS
        
numbers_df[1,which(colnames(numbers_df) %in% paste("age", c(first_age:n_ages_mal), sep=""))] <- landing_fleet[yy,2:(ncol(landing_fleet)-1)]                # modificare quiiiiiiiiiiiiiiiiiiii ( l'indice della matrice numbers: selezione colonne in base a tr)
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers <- numbers_df 
  
percs_table <-	data.frame(matrix(0, nrow=5, ncol=length( vector_ages)))
colnames(percs_table) <- c(paste("age",vector_ages, sep=""))  
rownames(percs_table) <- percentiles_numb 
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers.CI.perc <- percs_table	
for (PERC in 1:length(percentiles_numb)) {
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers.CI.perc[PERC,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers

}	

Inters[[yy]][[ALADYM_spe]]@totallanding@numbers <- Inters[[yy]][[ALADYM_spe]]@totallanding@numbers + Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers 
Inters[[yy]][[ALADYM_spe]]@totallanding@numbers.CI.perc[,] <- Inters[[yy]][[ALADYM_spe]]@totallanding@numbers
	# TOTAL_LANDING_numbers <- TOTAL_LANDING_numbers + numbers_df

  # ------------------------------------------------------------------- DISCARDS


#        if (is.na(INP$Discard[1,fleet_interaction_ord])) {
#        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers <- TOTAL_DISCARD_numbers_NA 
#        
#percs_table <-	data.frame(matrix(NA, nrow=5, ncol=length( c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)))))
#colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
#
#Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers.CI.perc <- percs_table				
#		
#		} else {

numbers_df[1,which(colnames(numbers_df) %in% paste("age", c(first_age:n_ages_mal), sep=""))] <- discard_fleet[yy,2:(ncol(discard_fleet)-1)]                # modificare quiiiiiiiiiiiiiiiiiiii ( l'indice della matrice numbers: selezione colonne in base a tr)  
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers <- numbers_df

percs_table <-	data.frame(matrix(0, nrow=5, ncol=length( vector_ages)))
colnames(percs_table) <- c(paste("age", vector_ages, sep=""))  
rownames(percs_table) <- percentiles_numb 
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers.CI.perc <- percs_table	
for (PERC in 1:length(percentiles_numb)) {
Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers.CI.perc[PERC,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers
}	

Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers + Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers 


Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers.CI.perc[,] <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers


#	   TOTAL_DISCARD_numbers <- TOTAL_DISCARD_numbers + numbers_df
#        }

     fleet_interaction_ord <- fleet_interaction_ord +1     
}

}

#
#
#Inters[[yy]][[ALADYM_spe]]@totallanding@numbers <- TOTAL_LANDING_numbers 
#Inters[[yy]][[ALADYM_spe]]@totallanding@numbers.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totallanding@numbers
#
#Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers <- TOTAL_DISCARD_numbers
#Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers 
#
  } # end years loop
                                                          # 564248826    == 69408988 + 43450094 + 241302576 +   78004097 +  9168448 + 46590044 +  75623492 +  701087.6

# total weigth of catches, landing and discard 
production_tbl <- read.csv(PRODUCTION_table, sep=";")
mortalities_tbl <- read.csv(MORTALITY_table, sep=";")

if (n_fleet_for_species > 1) {
catch_weight <- production_tbl[,4:(4 + n_fleet_for_species)]
catch_mean_length <- production_tbl[,(4 + n_fleet_for_species + 1):(4 + 2* n_fleet_for_species + 1)]
catch_mean_age <- production_tbl[,(4 + 2 * n_fleet_for_species + 2):(4 + 3* n_fleet_for_species + 2)]
landing_weight <- production_tbl[,(4 + 3 * n_fleet_for_species + 3):(4 + 4* n_fleet_for_species + 3)]
landing_mean_length <- production_tbl[,(4 + 4 * n_fleet_for_species + 4):(4 + 5* n_fleet_for_species + 4)]
landing_mean_age <- production_tbl[,(4 + 5 * n_fleet_for_species + 5):(4 + 6* n_fleet_for_species + 5)]
discard_weight <- production_tbl[,(4 + 6 * n_fleet_for_species + 6):(4 + 7* n_fleet_for_species + 6)]
discard_mean_length <- production_tbl[,(4 + 7 * n_fleet_for_species + 7):(4 + 8* n_fleet_for_species + 7)]
discard_mean_age <- production_tbl[,(4 + 8 * n_fleet_for_species + 8):(4 + 9* n_fleet_for_species + 8)]

} else {
catch_weight <- data.frame(cbind(production_tbl[,4], production_tbl[,4]))
colnames(catch_weight) <- c(colnames(production_tbl)[4], "X")
catch_mean_length <- data.frame(cbind(production_tbl[,5] , production_tbl[,5]))
colnames(catch_mean_length) <- c(colnames(production_tbl)[5], "X")
catch_mean_age <- data.frame(cbind(production_tbl[,6] , production_tbl[,6]))
colnames(catch_mean_age) <- c(colnames(production_tbl)[6],"X")
landing_weight <- data.frame(cbind(production_tbl[,7] , production_tbl[,7]))
colnames(landing_weight) <- c(colnames(production_tbl)[7], "X")
landing_mean_length <- data.frame(cbind(production_tbl[,8] , production_tbl[,8]))
colnames(landing_mean_length) <- c(colnames(production_tbl)[8], "X")
landing_mean_age <- data.frame(cbind(production_tbl[,9] , production_tbl[,9]))
colnames(landing_mean_age) <- c(colnames(production_tbl)[9], "X")
discard_weight <- data.frame(cbind(production_tbl[,10], production_tbl[,10]))
colnames(discard_weight) <- c(colnames(production_tbl)[10], "X")
discard_mean_length <- data.frame(cbind(production_tbl[,11] , production_tbl[,11]))
colnames(discard_mean_length) <- c(colnames(production_tbl)[11], "X")
discard_mean_age <- data.frame(cbind(production_tbl[,12] , production_tbl[,12]))
colnames(discard_mean_age) <- c(colnames(production_tbl)[12], "X")
}
 

   
  for (yy in 1:simperiod) {
  
  		rownames(percs_table_one_row) <- c(years[yy])
  
    Inters[[yy]][[ALADYM_spe]]@totalcatch@totalweight <- catch_weight$Total_Yield[yy]
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanLength <- catch_mean_length$Mean_length_in_catch[yy]
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanWeight <- lw_a * as.numeric(catch_mean_length$Mean_length_in_catch[yy])^ lw_b
    if (n_fleet_for_species > 1) {    
        Inters[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]
      } else {
        Inters[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality <-  mortalities_tbl[yy,ncol(mortalities_tbl)]
      }


    Inters[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc <- percs_table_one_row 
               
    Inters[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totalcatch@totalweight
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totalcatch@meanLength
    Inters[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totalcatch@meanWeight
    Inters[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality
 
    Inters[[yy]][[ALADYM_spe]]@totallanding@totalweight <- landing_weight$Total_Landing[yy]
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanLength <- landing_mean_length$Mean_length_in_Landing[yy]
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanWeight <- lw_a * as.numeric(landing_mean_length$Mean_length_in_Landing[yy])^ lw_b

    Inters[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc <- percs_table_one_row

	  Inters[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totallanding@totalweight
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totallanding@meanLength
    Inters[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totallanding@meanWeight

    loca_tot_disc <-  sum(Inters[[yy]][[ALADYM_spe]]@totaldiscard@numbers)

    Inters[[yy]][[ALADYM_spe]]@totaldiscard@totalweight <- discard_weight$Total_Discard[yy]
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanLength <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, discard_mean_length$Mean_length_in_Discard[yy])
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, (lw_a * as.numeric(discard_mean_length$Mean_length_in_Discard[yy])^ lw_b))
    
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc <- percs_table_one_row
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc <- percs_table_one_row
    
	  Inters[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@totalweight
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanLength
    Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight
    

		       
   fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {
if (n_fl %in% associated_fleetsegment_indices)  {

        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight <- as.numeric(catch_weight[yy,fleet_interaction_ord+1] )
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength <- as.numeric(catch_mean_length[yy,fleet_interaction_ord+1] )
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight <-  lw_a * as.numeric(catch_mean_length[yy,fleet_interaction_ord+1])^ lw_b
        
        if (n_fleet_for_species > 1) {        
  Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)]  
   } else {
       Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality <- mortalities_tbl[yy,ncol(mortalities_tbl)]
         }
        
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc <- percs_table_one_row
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc <- percs_table_one_row
      Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc <- percs_table_one_row
           Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc <- percs_table_one_row
              
		Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc[1,] <-  Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength
      Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc[1,] <-  Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc[1,] <-  Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality
		
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight <- as.numeric(landing_weight[yy,fleet_interaction_ord+1] )
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength <- as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight <-  lw_a * as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])^ lw_b
       
			     Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc <- percs_table_one_row
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc <- percs_table_one_row
      Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc <- percs_table_one_row
			   
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc[1,] <- as.numeric(landing_weight[yy,fleet_interaction_ord+1] )
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc[1,] <- as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc[1,] <-  lw_a * as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])^ lw_b
		 
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight <- ifelse(is.na(discard_weight[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_weight[yy,fleet_interaction_ord+1])) 
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_mean_length[yy,fleet_interaction_ord+1]))     
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, lw_a * as.numeric(discard_mean_length[yy,fleet_interaction_ord+1])^ lw_b )
  
   Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc <- percs_table_one_row
        Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc <- percs_table_one_row
      Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc <- percs_table_one_row
	

   Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength
    Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight
  

  
  if (fleet_interaction_ord == 1) {
        # weight, mean length and mean weight related to all the catches (not by fleet segment)
        Inters[[yy]][[ALADYM_spe]]@meanLength_catches <- as.numeric(catch_mean_length[yy,1])
     
		 Inters[[yy]][[ALADYM_spe]]@meanLength_catches.CI.perc <-  percs_table_one_row  
		Inters[[yy]][[ALADYM_spe]]@meanLength_catches.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@meanLength_catches 
				
        Inters[[yy]][[ALADYM_spe]]@meanWeight_catches <-  lw_a * as.numeric(catch_mean_length[yy,1]) ^ lw_b
        		 Inters[[yy]][[ALADYM_spe]]@meanWeight_catches.CI.perc <-  percs_table_one_row 
		Inters[[yy]][[ALADYM_spe]]@meanWeight_catches.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@meanWeight_catches
  }


        if (n_fleet_for_species > 1) {        
    Inters[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)]  
   } else {
     Inters[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,ncol(mortalities_tbl)] 
         }



    fleet_interaction_ord <-  fleet_interaction_ord +1
  } 
}

  }  # END YEARS LOOP


  
  
  for (yy in 1:simperiod) {


    
if (n_fleet_for_species > 1) {    
 Inters[[yy]][[ALADYM_spe]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]
 } else {
  Inters[[yy]][[ALADYM_spe]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,ncol(mortalities_tbl)]
 }
 
 Inters[[yy]][[ALADYM_spe]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,8]  
  }




# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE STOCKs in aladym
# ----------------------------------------------------------------------------

# L95 for stock
indicators_tbl <- read.csv(INDICATORS_table, sep=";")


population_tbl <- read.csv(POPULATION_table, sep=";")




for (yy in 1:simperiod) {
		rownames(percs_table_one_row) <- c(years[yy]) 

     # fleet stock interaction
     Inters[[yy]][[ALADYM_spe]]@L95_catches <-  indicators_tbl$catch_L0_95[yy]
     # exploited stock
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@L95 <- as.numeric(as.character(indicators_tbl$ex_stock_L0_95[yy]))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength <- as.numeric(as.character(indicators_tbl$Critical_length_expl_pop[yy]))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanLength <- as.numeric(as.character(population_tbl$Mean_length_of_exploited_pop[yy]))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate <- as.numeric(as.character(indicators_tbl$Harvest_ratio[yy] ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate <- as.numeric(as.character(indicators_tbl$Exploitation_rate[yy]))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight <- lw_a * Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanLength  ^ lw_b
     
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers <- population_tbl$NUMBERS_exploited_pop[yy]*1000
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB <- population_tbl$Total_biomass_exploited_pop[yy]
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers <- population_tbl$SS_NUMBERS_exploited_pop[yy]*1000
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB <- population_tbl$SSB_exploited_pop[yy]
	 
     # unexploited stock
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@L95 <- as.numeric(as.character(indicators_tbl$unex_stock_L0_95[yy] ))
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength <- as.numeric(as.character(indicators_tbl$Critical_length_unexpl_pop[yy] ))
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength <- as.numeric(as.character(population_tbl$Mean_length_of_unexploited_pop[yy] ))
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate <- 0
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate <- 0
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight <-  lw_a * Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength  ^ lw_b
     
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers <- population_tbl$NUMBERS_unexploited_pop[yy]*1000
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB <- population_tbl$Total_biomass_unexploited_pop[yy]
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers <- population_tbl$SS_NUMBERS_unexploited_pop[yy]*1000
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB <- population_tbl$SSB_unexploited_pop[yy]

	  Inters[[yy]][[ALADYM_spe]]@L95_catches.CI.perc <- percs_table_one_row  
	 Inters[[yy]][[ALADYM_spe]]@L95_catches.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@L95_catches  ))
     
		 
		 # exploited stock
     	   Inters[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc <- percs_table_one_row         
					Inters[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc<- percs_table_one_row  
					  Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc <- percs_table_one_row     
						Inters[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc <- percs_table_one_row  
						Inters[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc <- percs_table_one_row    
						 Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc <- percs_table_one_row  
						  
              Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc <- percs_table_one_row 
						 	  Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc <- percs_table_one_row   
		 Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc <- percs_table_one_row 
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row 

      

      			 	  
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@L95 ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanLength ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate )) 	
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc[1,] <-  as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate  ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight ))   	 
	   Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc[1,] <- as.numeric(as.character( Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB ))
 	   Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers ))
     Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc[1,] <- as.numeric(as.character( Inters[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers ))















     # unexploited stock
		 Inters[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc <-  percs_table_one_row
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc <- percs_table_one_row
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc <- percs_table_one_row
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc <- percs_table_one_row
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc <- percs_table_one_row
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc <- percs_table_one_row        	
     
              Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc <- percs_table_one_row 
						 	  Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc <- percs_table_one_row   
		 Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc <- percs_table_one_row 
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row  

		  
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc[1,] <-  Inters[[yy]][[ALADYM_spe]]@unexploitedStock@L95
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate 
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc[1,] <- Inters[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate 
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc[1,] <-  Inters[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight

     	   Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB ))
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc[1,] <- as.numeric(as.character( Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB ))
 	   Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc[1,] <- as.numeric(as.character(Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers ))
     Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc[1,] <- as.numeric(as.character( Inters[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers ))
}





ages_ <- c(0:(max(INP$MGrowth_tend , INP$FGrowth_tend )-1)) 
ex_stocknumbers_df_M <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(ex_stocknumbers_df_M) <- c(paste("age", ages_, sep=""))
colnames(ex_stocknumbers_df_M) <- MONTHS
ex_stocknumbers_df_F <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(ex_stocknumbers_df_F) <- c(paste("age", ages_, sep=""))
colnames(ex_stocknumbers_df_F) <- MONTHS
unex_stocknumbers_df_M <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(unex_stocknumbers_df_M) <- c(paste("age", ages_, sep=""))
colnames(unex_stocknumbers_df_M) <- MONTHS
unex_stocknumbers_df_F <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(unex_stocknumbers_df_F) <- c(paste("age", ages_, sep=""))
colnames(unex_stocknumbers_df_F) <- MONTHS

ex_stockSB_df_M <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(ex_stockSB_df_M) <- c(paste("age", ages_, sep=""))
colnames(ex_stockSB_df_M) <- MONTHS
ex_stockSB_df_F <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(ex_stockSB_df_F) <- c(paste("age", ages_, sep=""))
colnames(ex_stockSB_df_F) <- MONTHS
unex_stockSB_df_M <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(unex_stockSB_df_M) <- c(paste("age", ages_, sep=""))
colnames(unex_stockSB_df_M) <- MONTHS
unex_stockSB_df_F <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(unex_stockSB_df_F) <- c(paste("age", ages_, sep=""))
colnames(unex_stockSB_df_F) <- MONTHS

ex_SSB_df <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(ex_SSB_df) <- c(paste("age", ages_, sep=""))
colnames(ex_SSB_df) <- MONTHS
unex_SSB_df <- data.frame(matrix(0, nrow=(max(INP$MGrowth_tend , INP$FGrowth_tend )), ncol=length(MONTHS)))
rownames(unex_SSB_df) <- c(paste("age", ages_, sep=""))
colnames(unex_SSB_df) <- MONTHS

# total spawners males + female for exploited
ex_SS_numbers <- data.frame(matrix((BAS$MFSS_Number + BAS$FFSS_Number), nrow=1))
# total spawners males + female for unexploited
unex_SS_numbers <- data.frame(matrix((BAS$MUSS_Number + BAS$FUSS_Number), nrow=1))

# exploited stock numbers


for (ye in 1:simperiod) {


#print(paste("---------------------------------------------------- anno", years[ye]))
start_mon_simu <- (ye-1)*12+1 +1
end_mon_simu <- (ye*12) + 1

ind_month <- 1   
for (simu_mon in c(start_mon_simu:end_mon_simu)) {
#print(paste("Mese della simulazione", simu_mon))  
#print("MASCHI")

if (modulo(INP$tr,12) !=0 | INP$tr==0) {

for (ag in 1:INP$MGrowth_tend) {

  if (ag < (trunc(INP$tr/12)+1) ) {

         ex_stocknumbers_df_M[ag,ind_month] <-  0
         unex_stocknumbers_df_M[ag,ind_month] <- 0
         ex_stockSB_df_M[ag,ind_month] <- 0
         unex_stockSB_df_M[ag,ind_month] <- 0

  # print(paste("eta #", ag,"tutti NA"))
      } else {
      start_mon_age <- 1
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla              

 if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           # mort_vect_this_age <-  c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) , BAS$MM[start_mon_age:end_mon_age])
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
          # print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           # mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
        } 
#
#      if (INP$tr==0) {
#         mort_vect_this_age <- mort_vect_this_age[(length(mort_vect_this_age)-12+1):length(mort_vect_this_age)]
#      }


         ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      }
} 

} else {


for (ag in 1:INP$MGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]

            ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      } else {
           start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
       #   Populs[[ALADYM_spe]]@M.vect$F[ag,]  <- mort_vect_this_age
       
         ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

} 

} 

}


# FEMMINE

if (modulo(INP$tr,12) !=0 | INP$tr==0) {

for (ag in 1:INP$FGrowth_tend) {

  if (ag < (trunc(INP$tr/12)+1) ) {

         ex_stocknumbers_df_F[ag,ind_month] <-  0
         unex_stocknumbers_df_F[ag,ind_month] <- 0
         ex_stockSB_df_F[ag,ind_month] <- 0
         unex_stockSB_df_F[ag,ind_month] <- 0

  # print(paste("eta #", ag,"tutti NA"))
      } else {
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla     
     start_mon_age <- 1          
 if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           # mort_vect_this_age <-  c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) , BAS$MM[start_mon_age:end_mon_age])
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
          # print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           # mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
        } 
#
#      if (INP$tr==0) {
#         mort_vect_this_age <- mort_vect_this_age[(length(mort_vect_this_age)-12+1):length(mort_vect_this_age)]
#      }
         ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      }
} 

} else {


for (ag in 1:INP$FGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]

            ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      } else {
           start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
       #   Populs[[ALADYM_spe]]@M.vect$F[ag,]  <- mort_vect_this_age
       
         ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

} 

} 

}





ind_month <- ind_month +1
}   # end months of the year

      Inters[[ye]][[ALADYM_spe]]@exploitedStock@numbers$M <- ex_stocknumbers_df_M
      Inters[[ye]][[ALADYM_spe]]@exploitedStock@numbers$F <- ex_stocknumbers_df_F

      
      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@numbers$M <- unex_stocknumbers_df_M
      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@numbers$F <- unex_stocknumbers_df_F 
                                                                  
      Inters[[ye]][[ALADYM_spe]]@exploitedStock@SB$M <- ex_stockSB_df_M
      Inters[[ye]][[ALADYM_spe]]@exploitedStock@SB$F <- ex_stockSB_df_F
                                                


      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@SB$M <-  unex_stockSB_df_M
      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@SB$F <-  unex_stockSB_df_F
                                                
      SS_temp <- ex_SS_numbers[c(start_mon_simu:end_mon_simu)] 
      colnames(SS_temp) <- MONTHS
      rownames(SS_temp) <- years[ye]
      Inters[[ye]][[ALADYM_spe]]@exploitedStock@SS.numbers <- SS_temp
      SS_temp <-  unex_SS_numbers[c(start_mon_simu:end_mon_simu)]
      colnames(SS_temp) <- MONTHS
      rownames(SS_temp) <- years[ye]      
      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@SS.numbers <- SS_temp
      
      SSB_temp <- data.frame(matrix(SRO$FSSBiomass[c(start_mon_simu:end_mon_simu)], nrow=1))
      colnames(SSB_temp) <- MONTHS
      rownames(SSB_temp) <- years[ye]
      Inters[[ye]][[ALADYM_spe]]@exploitedStock@SSB <- SSB_temp
      SSB_temp <- data.frame(matrix(SRO$USSBiomass[c(start_mon_simu:end_mon_simu)], nrow=1))
      colnames(SSB_temp) <- MONTHS
      rownames(SSB_temp) <- years[ye]
      Inters[[ye]][[ALADYM_spe]]@unexploitedStock@SSB <- SSB_temp
}   # end year loop


# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE REFERENCE POINTS in aladym
# ----------------------------------------------------------------------------

# calculate_ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ALADYM_spe, sep=""),1])
use_ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ALADYM_spe, sep=""),2])
referencepoints_tbl <- try(read.csv(REFERENCEPOINTS_table, sep=";"), silent=TRUE)

#if ( ALADYM_RP | class(referencepoints_tbl) !=  "try-error" | SAtool != "NONE") {  
if (use_ALADYM_RP & class(referencepoints_tbl) !=  "try-error") {



referencepoints_tbl <- read.csv(REFERENCEPOINTS_table, sep=";")

# if the table has 4 rows the F0.1 corresponds to the row 3, else is not available

for (ye in 1:simperiod) {   















if (nrow(referencepoints_tbl) == 7) {
# there is no FMSY
# n.2 Fmax
 if (! is.na(as.numeric(referencepoints_tbl[2,2])) ) {
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,2] <- as.numeric(referencepoints_tbl[2,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,3] <- as.numeric(referencepoints_tbl[2,3]) * as.numeric(referencepoints_tbl[2,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,4] <- as.numeric(referencepoints_tbl[2,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,7] <- as.numeric(referencepoints_tbl[2,4]) * as.numeric(referencepoints_tbl[2,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,10] <- as.numeric(referencepoints_tbl[2,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,5] <- as.numeric(referencepoints_tbl[2,5])  * as.numeric(referencepoints_tbl[2,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,6] <- as.numeric(referencepoints_tbl[2,5])  # biomass per recruits
} else {
 Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,2] <- as.numeric(referencepoints_tbl[5,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,3] <- as.numeric(referencepoints_tbl[5,3]) * as.numeric(referencepoints_tbl[5,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,4] <- as.numeric(referencepoints_tbl[5,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,7] <- as.numeric(referencepoints_tbl[5,4]) * as.numeric(referencepoints_tbl[5,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,10] <- as.numeric(referencepoints_tbl[5,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,5] <- as.numeric(referencepoints_tbl[5,5])  * as.numeric(referencepoints_tbl[5,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,6] <- as.numeric(referencepoints_tbl[5,5])  # biomass per recruits
}

# Interactionsyear[[1]][[2]]@interactions[[1]]@catches

# n.3 F0.1
 if (! is.na(as.numeric(referencepoints_tbl[3,2])) ) {
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,2] <- as.numeric(referencepoints_tbl[3,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,3] <- as.numeric(referencepoints_tbl[3,3]) * as.numeric(referencepoints_tbl[3,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,4] <- as.numeric(referencepoints_tbl[3,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,7] <- as.numeric(referencepoints_tbl[3,4]) * as.numeric(referencepoints_tbl[3,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,10] <- as.numeric(referencepoints_tbl[3,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,5] <- as.numeric(referencepoints_tbl[3,5])  * as.numeric(referencepoints_tbl[3,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,6] <- as.numeric(referencepoints_tbl[3,5])  # biomass per recruits
} else {
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,2] <- as.numeric(referencepoints_tbl[6,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,3] <- as.numeric(referencepoints_tbl[6,3]) * as.numeric(referencepoints_tbl[6,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,4] <- as.numeric(referencepoints_tbl[6,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,7] <- as.numeric(referencepoints_tbl[6,4]) * as.numeric(referencepoints_tbl[6,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,10] <- as.numeric(referencepoints_tbl[6,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,5] <- as.numeric(referencepoints_tbl[6,5])  * as.numeric(referencepoints_tbl[6,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,6] <- as.numeric(referencepoints_tbl[6,5])  # biomass per recruits
}

# n.4 F0.2
 if (! is.na(as.numeric(referencepoints_tbl[4,2])) ) {
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,2] <- as.numeric(referencepoints_tbl[4,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,3] <- as.numeric(referencepoints_tbl[4,3]) * as.numeric(referencepoints_tbl[2,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,4] <- as.numeric(referencepoints_tbl[4,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,7] <- as.numeric(referencepoints_tbl[4,4]) * as.numeric(referencepoints_tbl[2,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,10] <- as.numeric(referencepoints_tbl[4,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,5] <- as.numeric(referencepoints_tbl[4,5])  * as.numeric(referencepoints_tbl[2,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,6] <- as.numeric(referencepoints_tbl[4,5])  # biomass per recruits
} else {
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,2] <- as.numeric(referencepoints_tbl[7,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,3] <- as.numeric(referencepoints_tbl[7,3]) * as.numeric(referencepoints_tbl[7,6])  # yield
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,4] <- as.numeric(referencepoints_tbl[7,3])   # yield per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,7] <- as.numeric(referencepoints_tbl[7,4]) * as.numeric(referencepoints_tbl[7,6])  # ssb
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,10] <- as.numeric(referencepoints_tbl[7,4])  # ssb per recruits
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,5] <- as.numeric(referencepoints_tbl[7,5])  * as.numeric(referencepoints_tbl[7,6])  # biomass
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,6] <- as.numeric(referencepoints_tbl[7,5])  # biomass per recruits
}

} else {
# there is only FMSY
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,2] <- as.numeric(referencepoints_tbl[2,2])
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,3] <- as.numeric(referencepoints_tbl[2,3])
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,7] <- as.numeric(referencepoints_tbl[2,4])
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,5] <- as.numeric(referencepoints_tbl[2,5])



} # end selection reference points
}




}






if ( use_ALADYM_RP & class(referencepoints_tbl) ==  "try-error" ) {
      print("ALADYM reference points table not found! Impossible to proceed with the DIAGNOSIS.", quote=F)
      print("Start again the software activating the reference points calculation in the case study configuration.", quote=F)
      showError("ALADYM reference points table not found! Impossible to proceed with the DIAGNOSIS.\nStart again the software activating the reference points calculation in the case study configuration.") 

   # error <- data.frame(matrix("ALADYM reference points table not found! Impossible to proceed with the DIAGNOSIS. Start again the software activating the reference points calculation in the case study configuration.", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\sim_info.err", sep=""), sep=";", col.names=F, row.names=F)

      ALADYM_rp_calc <- c(ALADYM_rp_calc, FALSE)
} else if  (!use_ALADYM_RP & (SAtool == "NONE" | SAtool == "SURBA")) {
      external_RPs_table_path <- as.character(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ALADYM_spe, sep=""),3] )
      external_RPs_table_path <- ifelse(length(external_RPs_table_path) == 0, "", external_RPs_table_path)
      external_RPs_table <- try(read.csv(external_RPs_table_path, sep=";", header=T), silent=TRUE)
        if ( class(external_RPs_table) ==  "try-error" ) {
            print("External reference points table not found! Impossible to proceed with the DIAGNOSIS.", quote=F)
            print("Start again the software inserting the path of the external reference points table in the case study configuration.", quote=F)
            showError("External reference points table not found! Impossible to proceed with the DIAGNOSIS.\nStart again the software inserting the path of the external reference points table in the case study configuration.") 
			
			   # error <- data.frame(matrix("External reference points table not found! Impossible to proceed with the DIAGNOSIS.\nStart again the software inserting the path of the external reference points table in the case study configuration.", ncol=1, nrow=1))
#		write.table(error, file = paste(casestudy_path, "\\sim_info.err", sep=""), sep=";", col.names=F, row.names=F)
		
            ALADYM_rp_calc <- c(ALADYM_rp_calc, FALSE)
        } else {
print("Reading REFERENCE POINTS from external table", quote=F)      
  external_RPs_table <- external_RPs_table[,2:ncol(external_RPs_table)]   
  print(external_RPs_table)       
  for (ye in 1:simperiod) {              
# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE REFERENCE POINTS
# ----------------------------------------------------------------------------
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 1] <-  as.numeric(as.character(external_RPs_table[1,1]))/ as.numeric(as.character(Inters[[ye]][[ALADYM_spe]]@mortalities[length(BMT_FLEETSEGMENTS)+1, 2])) # factor
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 2] <-  as.numeric(as.character(external_RPs_table[1,1]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 3] <-  as.numeric(as.character(external_RPs_table[1,2]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 4] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 3] /  as.numeric(as.character(external_RPs_table[1,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 5] <-  as.numeric(as.character(external_RPs_table[1,5]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 6] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 5] /  as.numeric(as.character(external_RPs_table[1,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@F0.1[3, 7] <-  as.numeric(as.character(external_RPs_table[1,4]))
 
#rp_temp_max <-  data.frame(matrix(0, ncol=7, nrow=1))
#colnames(rp_temp_max) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#                               # data.frame	values by items [factor, Y, Y/R, B, B/R, SSB]
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 1] <-  as.numeric(as.character(external_RPs_table[2,1]))/ as.numeric(as.character(Inters[[ye]][[ALADYM_spe]]@mortalities[length(BMT_FLEETSEGMENTS)+1, 2])) # factor
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 2] <-  as.numeric(as.character(external_RPs_table[2,1]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 3] <-  as.numeric(as.character(external_RPs_table[2,2]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 4] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 3] /  as.numeric(as.character(external_RPs_table[2,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 5] <-  as.numeric(as.character(external_RPs_table[2,5]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 6] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 5] /  as.numeric(as.character(external_RPs_table[2,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@Fmax[3, 7] <-  as.numeric(as.character(external_RPs_table[2,4]))


Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 1] <-  as.numeric(as.character(external_RPs_table[4,1]))/ as.numeric(as.character(Inters[[ye]][[ALADYM_spe]]@mortalities[length(BMT_FLEETSEGMENTS)+1, 2])) # factor
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 2] <-  as.numeric(as.character(external_RPs_table[4,1]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 3] <-  as.numeric(as.character(external_RPs_table[4,2]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 4] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 3] /  as.numeric(as.character(external_RPs_table[4,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 5] <-  as.numeric(as.character(external_RPs_table[4,5]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 6] <-  Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 5] /  as.numeric(as.character(external_RPs_table[4,3]))
Inters[[ye]][[ALADYM_spe]]@referencePoints@FMSY[3, 7] <-  as.numeric(as.character(external_RPs_table[4,4])) 
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ---------------------------------------------------------------------------- 
}            
}
} else {
      ALADYM_rp_calc <- c(ALADYM_rp_calc, TRUE)
}





# ALADYM to BMT:	INP	p_Production	-	starting from ALADYM format
   associated_fleetsegment <-as.vector(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)




 fleet_interaction_ord <- 1
for (n_fl in 1:n_fleet) {
if (n_fl %in% associated_fleetsegment_indices) {



for (ye in 1:simperiod) {



    start_mon_simu <- (ye-1)*12+1 +1
    end_mon_simu <- (ye*12) + 1
 pp_df <- data.frame(matrix(INP$p_Production[c(start_mon_simu:end_mon_simu), fleet_interaction_ord], nrow=1))
 colnames(pp_df) <- MONTHS
 rownames(pp_df) <- years[ye]
 Inters[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$pProduction <- pp_df
 fc_df <- data.frame(matrix(INP$Fishing_efforts[c(start_mon_simu:end_mon_simu), fleet_interaction_ord], nrow=1))
 colnames(fc_df) <- MONTHS
 rownames(fc_df) <- years[ye]
 Fleet[[ye]]@fleetsegments[[n_fl]]@fishingcoefficient <- fc_df
} # end years

    fleet_interaction_ord <-  fleet_interaction_ord +1
}
}  # end fleet loop


# INP	"INP$OPT_SG_TYPE, param1, param2, param3, param4, param5"
selectivity_df <- data.frame(matrix(0, nrow=12, ncol=6))
rownames(selectivity_df) <- MONTHS
colnames(selectivity_df) <- c("param1", "param2","param3","param4","param5", "param6")
# structure of selectivity data frame
#selectivity_list <- list(modelSelectivity="classical ogive", parameters=selectivity_df)

 fleet_interaction_ord <- 1
for (n_fl in 1:n_fleet) {
if (n_fl %in% associated_fleetsegment_indices) {



for (ye in 1:simperiod) {

    start_mon_simu <- (ye-1)*12+1 +1
    end_mon_simu <- (ye*12) + 1
    selectivity_df[,1] <- INP$param1[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,2] <- INP$param2[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,3] <- INP$param3[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,4] <- INP$param4[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,5] <- INP$param5[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,6] <- INP$OPT_SG_TYPE[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
		Inters[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$selectivity <- selectivity_df
		
		if (any(INP$Land_obl[c(start_mon_simu:end_mon_simu), fleet_interaction_ord] == "Y")) {
      Inters[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landing_obligation  <- "Y"
    } else {
    		Inters[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landing_obligation <- "N"
    }
 
		
#    selectivity_list <- list(code=INP$OPT_SG_TYPE[1,fleet_interaction_ord], parameters=selectivity_df)
#    Inters[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$selectivity <- selectivity_list
} # end years
         fleet_interaction_ord <-  fleet_interaction_ord +1
}
}  # end fleet loop  

#print(Inters[[1]][[1]]@referencePoints)

ret <- list(popus=Populs, inters=Inters, fleets=Fleet, refpoints=ALADYM_rp_calc)
return(ret)
}
