# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


   SS <- which(SS_TYPE == new_aldSimulation@spawners.ss)  # 1 o 2 
   min_ageM  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
   max_ageM <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
   min_ageF  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])
   max_ageF <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])

   INP$Average_forecast <- bmt_average_forecast
   
# ********************************************************************************* P-PRODUCTION
if (forecast < GLO$L_number) {
p_prod_temp = data.frame(matrix(0,nrow= (INP$Average_forecast*12+1), ncol=nb_gears))
p_prod_temp = data.frame(INP$p_Production[((forecast-INP$Average_forecast*12+1):forecast) ,] ) # considero i p production degli anni selezionati dall'utente
for (g in 1:nb_gears){
INP$p_Production[(forecast+1):(GLO$L_number + 1) ,g] = mean(as.numeric(as.character(p_prod_temp[,g])) )  # medie degli ultimi x anni (solo per il forecast)
}
}		

# *********************************************************************************  MATRICI DELLA SELETTIVITà PER ETà E LUNGHEZZA (INIZIALIZZAZIONE)
n_ages_M <- INP$MGrowth_tend  
n_ages_F <- INP$FGrowth_tend  
first_age_mal <- 0
first_age_fem <- 0
n_ages_M <- n_ages_M - trunc(INP$tr/12)
first_age_mal <- trunc(INP$tr/12)
n_ages_F <- n_ages_F - trunc(INP$tr/12)
first_age_fem <- trunc(INP$tr/12)
l_inf_M <- INP$MGrowth_Linf_max 
l_inf_F <- INP$FGrowth_Linf_max 
l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))	
	
print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 

nrow_males <- length(years_forecast)*n_ages_M        #
nrow_females <- length(years_forecast)*n_ages_F       #
nrow_males_lenXsel <- length(years_forecast)*length(l_inf_lens_M)        #
nrow_females_lenXsel <- length(years_forecast)*length(l_inf_lens_F)       #
INP$Sel_vector_fore <- data.frame(matrix(nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))                           
INP$Sel_vector_len_fore <- data.frame(matrix(nrow=(nrow_males_lenXsel + nrow_females_lenXsel), ncol=(3+length(FLEETSEGMENTS_names))))						  						  
INP$Sel_vector_fore[,1] <- c(rep(c(first_age_mal:(n_ages_M+first_age_mal-1)), length(years_forecast)), rep(c(first_age_fem:(n_ages_F+first_age_fem-1)), length(years_forecast)))
INP$Sel_vector_len_fore[,1] <- c(rep(l_inf_lens_M, length(years_forecast)), rep(l_inf_lens_F, length(years_forecast)))

lab_year <- rep(years_forecast,n_ages_M)
lab_year <- lab_year[order(lab_year)]  # M
INP$Sel_vector_fore[1:nrow_males,2] <- lab_year
lab_year <- rep(years_forecast,n_ages_F)
lab_year <- lab_year[order(lab_year)]  # F
INP$Sel_vector_fore[(nrow_males+1):(nrow_males+nrow_females),2] <- lab_year

lab_year <- rep(years_forecast,length(l_inf_lens_M))
lab_year <- lab_year[order(lab_year)]  # M
INP$Sel_vector_len_fore[1:nrow_males_lenXsel,2] <- lab_year
lab_year <- rep(years_forecast,length(l_inf_lens_F))
lab_year <- lab_year[order(lab_year)]  # F
INP$Sel_vector_len_fore[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),2] <- lab_year

INP$typeEffFishMort <- rep("", nb_gears)
INP$COEF1 <- rep(-1 ,nb_gears)
INP$COEF2 <- rep(-1 ,nb_gears)
INP$Type <-  rep(NA, nb_gears)

# *********************************************************************************  MATRICI DELLA SELETTIVITà PER ETà E LUNGHEZZA (ASSEGNAZIONE)
for (gear in 1:length(FLEETSEGMENTS_names)) {   
if (new_aldSimulation@enteringMortality != "F") {
if (FleetList_forecast[[gear]]@selectivity.mode == "age") {
     INP$Type[gear] <- "A"
 } else if (FleetList_forecast[[gear]]@selectivity.mode == "length") {
    INP$Type[gear] <- "L"
 }

if (nrow(FleetList_forecast[[gear]]@SelectivityAge.M.vector) == 0 ) {
  SelAGE_males_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_M+1)))
  colnames(SelAGE_males_fs) <- c("years", paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
  SelAGE_males_fs$years <- years_forecast
} else {
 SelAGE_males_fs <-   FleetList_forecast[[gear]]@SelectivityAge.M.vector
}
    
if (nrow(FleetList_forecast[[gear]]@SelectivityAge.F.vector) == 0 ) {
  SelAGE_females_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_F+1)))
  colnames(SelAGE_females_fs) <- c("years", paste("age", c(first_age_fem:(n_ages_F-1)), sep="") )
  SelAGE_females_fs$years <- years_forecast
} else {
  SelAGE_females_fs <-   FleetList_forecast[[gear]]@SelectivityAge.F.vector
}

   if (nrow(FleetList_forecast[[gear]]@SelectivityLength.M.vector) == 0 ) {
    SelLENGTH_males_fs <- data.frame( matrix(NA, nrow=length(l_inf_lens_M), ncol=(length(years_forecast)+2) ) )
  colnames(SelLENGTH_males_fs) <- c("Length", years_forecast, sep="")
  SelLENGTH_males_fs$Length <- l_inf_lens_M
} else {
 SelLENGTH_males_fs <-   FleetList_forecast[[gear]]@SelectivityLength.M.vector
}

if (nrow(FleetList_forecast[[gear]]@SelectivityLength.F.vector) == 0 ) {
  SelLENGTH_females_fs <- data.frame(matrix(NA, nrow=length(l_inf_lens_F), ncol=(length(years_forecast)+2) ) )
  colnames(SelLENGTH_females_fs) <- c("Length", years_forecast, sep="")
   SelLENGTH_females_fs$Length <- l_inf_lens_F
} else {
  SelLENGTH_females_fs <-   FleetList_forecast[[gear]]@SelectivityLength.F.vector
}
   }
   
 if (exists("col_to_add_M_SelAGE_Vector")) { rm(col_to_add_M_SelAGE_Vector) }
if (exists("col_to_add_F_SelAGE_Vector")) { rm(col_to_add_F_SelAGE_Vector) }  

  if (exists("col_to_add_M_SelLENGTH_Vector")) { rm(col_to_add_M_SelLENGTH_Vector) }
if (exists("col_to_add_F_SelLENGTH_Vector")) { rm(col_to_add_F_SelLENGTH_Vector) }   
 
 for (yy in 1:length(years_forecast) ) {
 if (new_aldSimulation@enteringMortality != "F") {    
   if (FleetList_forecast[[gear]]@selectivity.mode == "age") {
if (!exists("col_to_add_M_SelAGE_Vector")) {
col_to_add_M_SelAGE_Vector <-  as.numeric(as.character(SelAGE_males_fs[SelAGE_males_fs$Year == years_forecast[yy],2:(n_ages_M+1)]))
} else {
col_to_add_M_SelAGE_Vector <- c( col_to_add_M_SelAGE_Vector, as.numeric(as.character(SelAGE_males_fs[SelAGE_males_fs$Year == years_forecast[yy],2:(n_ages_M+1)])) )
}
     
if (!exists("col_to_add_F_SelAGE_Vector")) {
col_to_add_F_SelAGE_Vector <-  as.numeric(as.character(SelAGE_females_fs[SelAGE_females_fs$Year == years_forecast[yy],2:(n_ages_F+1)]))
} else {
col_to_add_F_SelAGE_Vector <- c(col_to_add_F_SelAGE_Vector,as.numeric(as.character(SelAGE_females_fs[SelAGE_females_fs$Year == years_forecast[yy],2:(n_ages_F+1)])) )
}

} else if (FleetList_forecast[[gear]]@selectivity.mode == "length") {

if (!exists("col_to_add_M_SelLENGTH_Vector")) {
col_to_add_M_SelLENGTH_Vector <-  as.numeric(as.character(SelLENGTH_males_fs[, yy+1]))
} else {
col_to_add_M_SelLENGTH_Vector <- c( col_to_add_M_SelLENGTH_Vector, as.numeric(as.character(SelLENGTH_males_fs[, yy+1]) ) )
}
     
if (!exists("col_to_add_F_SelLENGTH_Vector")) {
col_to_add_F_SelLENGTH_Vector <-  as.numeric(as.character(SelLENGTH_females_fs[, yy+1]))
} else {
col_to_add_F_SelLENGTH_Vector <- c(col_to_add_F_SelLENGTH_Vector,as.numeric(as.character(SelLENGTH_females_fs[, yy+1])) )
}

}

}
}


if (new_aldSimulation@enteringMortality != "F") {
if (FleetList_forecast[[gear]]@selectivity.mode == "age") {
INP$Sel_vector_fore[1:(nrow_males),(2+gear)] <- col_to_add_M_SelAGE_Vector
INP$Sel_vector_fore[(nrow_males+1):(nrow_males+nrow_females),(2+gear)]  <- col_to_add_F_SelAGE_Vector

} else if (FleetList_forecast[[gear]]@selectivity.mode == "length") {
INP$Sel_vector_len_fore[1:(nrow_males_lenXsel),(2+gear)] <- col_to_add_M_SelLENGTH_Vector
INP$Sel_vector_len_fore[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),(2+gear)]  <- col_to_add_F_SelLENGTH_Vector 
}    
}

if (FleetList_forecast[[gear]]@EffortF.relationship$relationship_type == "L") {
    INP$typeEffFishMort[gear] <- "LIN"
} else if (FleetList_forecast[[gear]]@EffortF.relationship$relationship_type == "P") {
	  INP$typeEffFishMort[gear] <- "EXP"
}

#INP$typeEffFishMort = c("LIN", "EXP","EXP","EXP","EXP","EXP","EXP","LIN")# rep("LIN",8) #or "EXP"
INP$COEF1[gear] <-  as.numeric(as.character(FleetList_forecast[[gear]]@EffortF.relationship$a))
INP$COEF2[gear] <- as.numeric(as.character(FleetList_forecast[[gear]]@EffortF.relationship$b))

for (loca_i in 1:(12*foreperiod)) {  
  
  if (as.numeric(INP$OPT_F_TYPE)==1) {

  if (FleetList_forecast[[gear]]@selectivity.mode == "params") {
  INP$param1[loca_i+forecast,gear]  <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 3]
   INP$param2[loca_i+forecast,gear]  <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 4]

   INP$param3[loca_i+forecast,gear]  <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 5]
   INP$param4[loca_i+forecast,gear]  <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 6]
   INP$param5[loca_i+forecast,gear]  <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 7]
   INP$OPT_SG_TYPE[loca_i+forecast, gear] <- FleetList_forecast[[gear]]@selectivity.vector[loca_i, 8]
   } else {
   INP$OPT_SG_TYPE[loca_i+forecast, gear] <- 7
   }
  }  
  }

for (loca_i in 1:(12*foreperiod)) {  
    if (FleetList_forecast[[gear]]@discard.calculation == "YES") {
 if (FleetList_forecast[[gear]]@discard.datatype == "Reverse ogive") {
    INP$Type_discard[loca_i+forecast,gear]  <- 1
   INP$param6[loca_i+forecast,gear]  <- as.numeric(as.character(FleetList_forecast[[gear]]@discard.vector[loca_i, 3]))
   INP$param7[loca_i+forecast,gear]  <- as.numeric(as.character(FleetList_forecast[[gear]]@discard.vector[loca_i, 4] ))
   } else {
    INP$Type_discard[loca_i+forecast,gear] <- 2
    INP$param6[loca_i+forecast,gear]  <- NA
   INP$param7[loca_i+forecast,gear]  <- NA 
    }
}
   disc_calc <-   FleetList_forecast[[gear]]@discard.calculation
   INP$Discard[loca_i+forecast,gear] <- ifelse(disc_calc == "YES", "Y", ifelse(disc_calc == "0", 0, NA))      
   }
 }
 
# *********************************************************************************  discard and survivability (INIZIALIZZAZIONE)
 
INP$Survivability_monthly_vec   <-  rep(as.numeric(as.character(new_aldSimulation@monthlysurvivability)), length(c(years,years_forecast))) 

INP$Sel_vector_fore[1:(nrow_males),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$Sel_vector_fore[(nrow_males+1):(nrow_males+nrow_females),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$Sel_vector_fore) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")

INP$Sel_vector_len_fore[1:(nrow_males_lenXsel),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$Sel_vector_len_fore[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$Sel_vector_len_fore) <- c("Length", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")

INP$D_Vector_fore <- data.frame(matrix(0, nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))
INP$D_Vector_fore[,1] <- c(rep(c(first_age_mal:(n_ages_M+first_age_mal-1)), length(years_forecast)), rep(c(first_age_fem:(n_ages_F+first_age_fem-1)), length(years_forecast)))

lab_year <- rep(years_forecast,n_ages_M)
lab_year <- lab_year[order(lab_year)]  # M
INP$D_Vector_fore[1:nrow_males,2] <- lab_year
lab_year <- rep(years_forecast,n_ages_F)
lab_year <- lab_year[order(lab_year)]  # F
INP$D_Vector_fore[(nrow_males+1):(nrow_males+nrow_females),2] <- lab_year

INP$Disc_Surv_rate_fore_temp <- c()
INP$Disc_Surv_rate_type_fore_temp <- c()
INP$Disc_Surv_rate_constF_fore_temp <- c()
INP$Disc_Surv_rate_constM_fore_temp <- c()
INP$Disc_Surv_rate_ogive_param1_fore_temp <- c()
INP$Disc_Surv_rate_ogive_param2_fore_temp <- c()

INP$Esc_Surv_rate_fore_temp <- c()
INP$Esc_Surv_rate_type_fore_temp <- c()
INP$Esc_Surv_rate_constM_fore_temp <- c()
INP$Esc_Surv_rate_constF_fore_temp <- c()
INP$Esc_Surv_rate_ogive_param1_fore_temp <- c()
INP$Esc_Surv_rate_ogive_param2_fore_temp <- c()  #INP$Sel_vector_fore <- data.frame(matrix(nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))
INP$Esc_Surv_rate_vectM_fore_temp <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Esc_Surv_rate_vectF_fore_temp <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

n_ages_M <- INP$MGrowth_tend  
n_ages_F <- INP$FGrowth_tend  
first_age_mal <- 0
first_age_fem <- 0

n_ages_M <- n_ages_M - trunc(INP$tr/12)
first_age_mal <- trunc(INP$tr/12)
n_ages_F <- n_ages_F - trunc(INP$tr/12)
first_age_fem <- trunc(INP$tr/12)
    
INP$Esc_Vector_fore_temp <- data.frame(matrix(NA, nrow=(n_ages_M + n_ages_F),ncol=(length(FLEETSEGMENTS_names)+2)))
colnames(INP$Esc_Vector_fore_temp) <- c("Age", paste("fs", 1:length(FLEETSEGMENTS_names), sep=""), "Sex")
INP$Esc_Vector_fore_temp[,1] <- c(c(first_age_mal:(n_ages_M+first_age_mal-1)), c(first_age_fem:(n_ages_F+first_age_fem-1)))
INP$Esc_Vector_fore_temp[,ncol(INP$Esc_Vector_fore_temp)] <- c(rep("M", n_ages_M), rep("F", n_ages_F))


# *********************************************************************************  discard and survivability (ASSEGNAZIONE)
for (gear in 1:nb_gears) {
if (as.character(FleetList_forecast[[gear]]@discard.calculation) != "NA") {
if (FleetList_forecast[[gear]]@discard.datatype == "Reverse ogive" ) {
    INP$D_Vector_males_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_M+1)))
  colnames(INP$D_Vector_males_fs) <- c("years", paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
  INP$D_Vector_males_fs$years <- years_forecast
} else {
 INP$D_Vector_males_fs <-   FleetList_forecast[[gear]]@discard_extvector.M.vector
}

} else {
INP$D_Vector_males_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_M+1)))
  colnames(INP$D_Vector_males_fs) <- c("years", paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
  INP$D_Vector_males_fs$years <- years_forecast
}

if (as.character(FleetList_forecast[[gear]]@discard.calculation) != "NA") {
if (FleetList_forecast[[gear]]@discard.datatype == "Reverse ogive") {
  INP$D_Vector_females_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_F+1)))
  colnames(INP$D_Vector_females_fs) <- c("years", paste("age", c(first_age_fem:(n_ages_F-1)), sep="") )
  INP$D_Vector_females_fs$years <- years_forecast
} else {
  INP$D_Vector_females_fs <-   FleetList_forecast[[gear]]@discard_extvector.F.vector
}
} else {
  INP$D_Vector_females_fs <- data.frame(matrix(NA, nrow=length(years_forecast), ncol=(n_ages_F+1)))
  colnames(INP$D_Vector_females_fs) <- c("years", paste("age", c(first_age_fem:(n_ages_F-1)), sep="") )
  INP$D_Vector_females_fs$years <- years_forecast
}



if (exists("col_to_add_M_D_Vector")) { rm(col_to_add_M_D_Vector) }
if (exists("col_to_add_F_D_Vector")) { rm(col_to_add_F_D_Vector) }

for (yy in 1:length(years_forecast) ) {
if (!exists("col_to_add_M_D_Vector")) {
col_to_add_M_D_Vector <-  as.numeric(as.character(INP$D_Vector_males_fs[INP$D_Vector_males_fs$year == years_forecast[yy],2:(n_ages_M+1)]))
} else {
col_to_add_M_D_Vector <- c( col_to_add_M_D_Vector, as.numeric(as.character(INP$D_Vector_males_fs[INP$D_Vector_males_fs$year == years_forecast[yy],2:(n_ages_M+1)])) )
}
     
if (!exists("col_to_add_F_D_Vector")) {
col_to_add_F_D_Vector <-  as.numeric(as.character(INP$D_Vector_females_fs[INP$D_Vector_females_fs$year == years_forecast[yy],2:(n_ages_F+1)]))
} else {
col_to_add_F_D_Vector <- c(col_to_add_F_D_Vector,as.numeric(as.character(INP$D_Vector_females_fs[INP$D_Vector_females_fs$year == years_forecast[yy],2:(n_ages_F+1)])) )
}

}

INP$D_Vector_fore[1:(nrow_males),(2+gear)] <- col_to_add_M_D_Vector
INP$D_Vector_fore[(nrow_males+1):(nrow_males+nrow_females),(2+gear)] <- col_to_add_F_D_Vector

# da qui

INP$Disc_Surv_rate_fore_temp <- c(INP$Disc_Surv_rate_fore_temp, as.character(  FleetList_forecast[[gear]]@discard.survivability.calculation ))
INP$Esc_Surv_rate_fore_temp <- c(INP$Esc_Surv_rate_fore_temp, as.character( FleetList_forecast[[gear]]@escape.survivability.calculation ))

  if ( length(FleetList_forecast[[gear]]@discard.survivability.datatype) > 0) {
       if (FleetList_forecast[[gear]]@discard.survivability.datatype == "C") {
               INP$Disc_Surv_rate_type_fore_temp <- c(INP$Disc_Surv_rate_type_fore_temp, 1 ) # 1 constant
       } else if (FleetList_forecast[[gear]]@discard.survivability.datatype == "DOS") {
                 INP$Disc_Surv_rate_type_fore_temp <- c(INP$Disc_Surv_rate_type_fore_temp, 2 ) # 2 ogive
       } else {
           INP$Disc_Surv_rate_type_fore_temp <- c(INP$Disc_Surv_rate_type_fore_temp,NA ) # NA
       }
  } else {
     INP$Disc_Surv_rate_type_fore_temp <- c(INP$Disc_Surv_rate_type_fore_temp,NA ) # 1 constant, 2 ogive
  }

    if ( length(FleetList_forecast[[gear]]@escape.survivability.datatype) > 0) {
if (FleetList_forecast[[gear]]@escape.survivability.datatype == "C") {
          INP$Esc_Surv_rate_type_fore_temp <-  c(INP$Esc_Surv_rate_type_fore_temp, 1 ) # 1 constant
} else if (FleetList_forecast[[gear]]@escape.survivability.datatype == "DOS" ) {    
       if (FleetList_forecast[[gear]]@escape.survivability.DOS.datatype == "O") {
            INP$Esc_Surv_rate_type_fore_temp <-  c(INP$Esc_Surv_rate_type_fore_temp, 2 ) # 2 ogive 
       } else if (FleetList_forecast[[gear]]@escape.survivability.DOS.datatype  == "V") {
             INP$Esc_Surv_rate_type_fore_temp <-  c(INP$Esc_Surv_rate_type_fore_temp, 3 )   # 3 externat vector
       }
} else {
     INP$Esc_Surv_rate_type_fore_temp <-  c(INP$Esc_Surv_rate_type_fore_temp, NA) # NA
} 
} else {
     INP$Esc_Surv_rate_type_fore_temp <-  c(INP$Esc_Surv_rate_type_fore_temp, NA) # NA
}

INP$Disc_Surv_rate_constM_fore_temp <- c(INP$Disc_Surv_rate_constM_fore_temp, ifelse(as.character( FleetList_forecast[[gear]]@discard.survivability.datatype) == "C", as.numeric(as.character(FleetList_forecast[[gear]]@discard.survivability.params$param1_or_M[1])), NA) ) # param 1 / males 
INP$Disc_Surv_rate_constF_fore_temp <- c(INP$Disc_Surv_rate_constF_fore_temp, ifelse(as.character( FleetList_forecast[[gear]]@discard.survivability.datatype) == "C", as.numeric(as.character(FleetList_forecast[[gear]]@discard.survivability.params$param2_or_F[1])), NA) ) # param 1 / males 

if ( nrow(FleetList_forecast[[gear]]@escape.survivability.constant) > 0 )  {
     INP$Esc_Surv_rate_constM_fore_temp <- c(INP$Esc_Surv_rate_constM_fore_temp,  as.numeric(as.character(FleetList_forecast[[gear]]@escape.survivability.constant$males[1])))
     INP$Esc_Surv_rate_constF_fore_temp <- c(INP$Esc_Surv_rate_constF_fore_temp,  as.numeric(as.character(FleetList_forecast[[gear]]@escape.survivability.constant$females[1])))
} else {
     INP$Esc_Surv_rate_constM_fore_temp <- c(INP$Esc_Surv_rate_constM_fore_temp, NA)
     INP$Esc_Surv_rate_constF_fore_temp <- c(INP$Esc_Surv_rate_constF_fore_temp, NA)
}

if (nrow(FleetList_forecast[[gear]]@escape.survivability.DOS.ogiveparams) > 0) {
     INP$Esc_Surv_rate_ogive_param1_fore_temp <- c(INP$Esc_Surv_rate_ogive_param1_fore_temp, as.numeric(as.character(FleetList_forecast[[gear]]@escape.survivability.DOS.ogiveparams$param1[1])))
     INP$Esc_Surv_rate_ogive_param2_fore_temp <- c(INP$Esc_Surv_rate_ogive_param2_fore_temp, as.numeric(as.character(FleetList_forecast[[gear]]@escape.survivability.DOS.ogiveparams$param2[1])))
} else {
     INP$Esc_Surv_rate_ogive_param1_fore_temp <- c(INP$Esc_Surv_rate_ogive_param1_fore_temp, NA)
     INP$Esc_Surv_rate_ogive_param2_fore_temp <- c(INP$Esc_Surv_rate_ogive_param2_fore_temp, NA)
}

INP$Disc_Surv_rate_ogive_param1_fore_temp <- c(INP$Disc_Surv_rate_ogive_param1_fore_temp, ifelse(as.character( FleetList_forecast[[gear]]@discard.survivability.datatype) == "DOS", as.numeric(as.character(FleetList_forecast[[gear]]@discard.survivability.params$param1_or_M[1])), NA) ) # param 1 / males 
INP$Disc_Surv_rate_ogive_param2_fore_temp <- c(INP$Disc_Surv_rate_ogive_param2_fore_temp, ifelse(as.character( FleetList_forecast[[gear]]@discard.survivability.datatype) == "DOS", as.numeric(as.character(FleetList_forecast[[gear]]@discard.survivability.params$param2_or_F[1])), NA) ) # param 1 / males 

}   # °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° end GEAR


INP$Disc_Surv_rate_fore <- INP$Disc_Surv_rate_fore_temp
INP$Disc_Surv_rate_type_fore <- INP$Disc_Surv_rate_type_fore_temp

INP$Disc_Surv_rate_constF_fore <- INP$Disc_Surv_rate_constF_fore_temp
INP$Disc_Surv_rate_constM_fore <- INP$Disc_Surv_rate_constM_fore_temp

INP$Disc_Surv_rate_ogive_param2_fore <- INP$Disc_Surv_rate_ogive_param2_fore_temp
INP$Disc_Surv_rate_ogive_param1_fore <- INP$Disc_Surv_rate_ogive_param1_fore_temp

INP$Disc_Surv_rate_vectM_fore <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Disc_Surv_rate_vectF_fore <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

INP$Esc_Surv_rate_vectM_fore <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Esc_Surv_rate_vectF_fore <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

INP$Esc_Surv_rate_fore <- INP$Esc_Surv_rate_fore_temp
INP$Esc_Surv_rate_type_fore <- INP$Esc_Surv_rate_type_fore_temp

INP$Esc_Surv_rate_constM_fore <- INP$Esc_Surv_rate_constM_fore_temp
INP$Esc_Surv_rate_constF_fore <- INP$Esc_Surv_rate_constF_fore_temp

INP$Esc_Surv_rate_ogive_param1_fore <-  INP$Esc_Surv_rate_ogive_param1_fore_temp
INP$Esc_Surv_rate_ogive_param2_fore <-  INP$Esc_Surv_rate_ogive_param2_fore_temp

for (i in c(1:length(FLEETSEGMENTS_names))) {
INP$Disc_Surv_rate_vectM_fore[,i] = logistic(1,INP$Disc_Surv_rate_ogive_param1_fore[i],INP$Disc_Surv_rate_ogive_param2_fore[i],BAS$MLength)
INP$Disc_Surv_rate_vectF_fore[,i]= logistic(1,INP$Disc_Surv_rate_ogive_param1_fore[i],INP$Disc_Surv_rate_ogive_param2_fore[i],BAS$FLength)

if (INP$Esc_Surv_rate_fore[i] == "Y") {
if (INP$Esc_Surv_rate_type_fore[i] == 2)  {
INP$Esc_Surv_rate_vectM_fore[,i] = logistic(1,INP$Esc_Surv_rate_ogive_param1_fore[i],INP$Esc_Surv_rate_ogive_param2_fore[i],BAS$MLength)
INP$Esc_Surv_rate_vectF_fore[,i]= logistic(1,INP$Esc_Surv_rate_ogive_param1_fore[i],INP$Esc_Surv_rate_ogive_param2_fore[i],BAS$FLength)
} else if (INP$Esc_Surv_rate_type_fore[i] == 3)  {
INP$Esc_Vector_fore_temp[INP$Esc_Vector_fore_temp$Sex == "M",i+1] <- as.numeric(as.character(FleetList_forecast[[i]]@escape.survivability.DOS.ext_vect.M[1,])) 
INP$Esc_Vector_fore_temp[INP$Esc_Vector_fore_temp$Sex == "F",i+1] <- as.numeric(as.character(FleetList_forecast[[i]]@escape.survivability.DOS.ext_vect.F[1,]))
}
} 

}

INP$Disc_Surv_rate_vectM_fore = INP$Disc_Surv_rate_vectM_fore[-1,]
INP$Disc_Surv_rate_vectF_fore = INP$Disc_Surv_rate_vectF_fore[-1,]

INP$Esc_Surv_rate_vectM_fore = INP$Esc_Surv_rate_vectM_fore[-1,]
INP$Esc_Surv_rate_vectF_fore = INP$Esc_Surv_rate_vectF_fore[-1,]

#FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.M
#FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.F

INP$Esc_Vector_fore <- INP$Esc_Vector_fore_temp    # matrix with all the gears
INP$Esc_vectorM_fore <- INP$Esc_Vector_fore[as.character(INP$Esc_Vector_fore$Sex)=="M",]        # by sex
INP$Esc_vectorF_fore <- INP$Esc_Vector_fore[as.character(INP$Esc_Vector_fore$Sex)=="F",]


INP$D_Vector_fore[1:(nrow_males),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$D_Vector_fore[(nrow_males+1):(nrow_males+nrow_females),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$D_Vector_fore) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")


INP$D_vectorM_fore = INP$D_Vector_fore[as.character(INP$D_Vector_fore$Sex)=="M",]
INP$D_vectorF_fore = INP$D_Vector_fore[as.character(INP$D_Vector_fore$Sex)=="F",]

write.table(INP$D_Vector_fore, file= DISCARD_BYAGE_EXTERNALVECT_INP_table, sep=";", row.names=F)


INP$Sel_vectorF_fore <- INP$Sel_vector_fore[INP$Sel_vector_fore[,ncol(INP$Sel_vector_fore)] == "F", ]
INP$Sel_vectorM_fore <- INP$Sel_vector_fore[INP$Sel_vector_fore[,ncol(INP$Sel_vector_fore)] == "M", ]


INP$Sel_vector_lenF_fore <- INP$Sel_vector_len_fore[INP$Sel_vector_len_fore[,ncol(INP$Sel_vector_len_fore)] == "F", ]
INP$Sel_vector_lenM_fore <- INP$Sel_vector_len_fore[INP$Sel_vector_len_fore[,ncol(INP$Sel_vector_len_fore)] == "M", ]



# ********************************************************************************* LANDING OBLIGATION
for (gear in 1:nb_gears) {
this_land_obl <- data.frame(FleetList_forecast[[gear]]@landing.obligation.vector)
 landobl_temp <- c()
       for (y in 1:length(years_forecast)) {
      if (as.character(FleetList_forecast[[gear]]@discard.calculation) == "YES") {
       row_obl <- as.character(this_land_obl[y, colnames(this_land_obl) != "year"]  )
       } else {
       row_obl <- rep("N", 12)
       }
       landobl_temp <- c(landobl_temp, row_obl)
       }
        INP$Land_obl[(forecast+1):((12*foreperiod)+forecast),gear] <- landobl_temp[1:(12*foreperiod)]                                                # <<----------------------------- NEW      
}



# *********************************************************************************************+ VARIABILI DI SFORZO E FISHING COEFFICIENT

if (!IN_BEMTOOL)  {
if (gtkToggleButtonGetActive(radio_effortdata) ) {
eff_data <- get_effort_data_fore() 
fishing_eff_result <- fact_calc(eff_data,"N")
  for (gear in 1:nb_gears) {
            for (i in 1:forecast){
            INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]         
            }
        }
} else {
  #eff_data <- get_effort_data_fore() 
  INP$Fishing_efforts <- get_fishing_coefficient_fore() 
}

}  else {   
# bemtool
  if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY) {
 eff_data <- get_effort_data_fore() 
fishing_eff_result <- fact_calc(eff_data,"N")
  for (gear in 1:nb_gears) {
            for (i in 1:forecast){
            INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]         
            }
        }
 }       
}

 
if (FleetList_forecast[[1]]@production.datatype == "Production data") {
prod_data <- get_production_data()
     
for (i in 1:forecast){
    INP$p_Production[i,] = P_production_calc(prod_data,forecast)[i,]
    write.table(INP$p_Production, file=P_PRODUCTION_DATA_working_table, sep=";", row.names=F)
}

} else {
    INP$p_Production <- get_p_production()
}







for (nc in 1:ncol(INP$p_Production) ) {
 pProdForMean <- data.frame(matrix(ncol=INP$Average_forecast, nrow=12)) 
#

for (ny in 1:INP$Average_forecast) {
   pProdForMean[,ny] <-  as.numeric(as.character(INP$p_Production[c((forecast-(INP$Average_forecast-ny+1)*12+1):(forecast-(INP$Average_forecast-ny)*12)),nc]))
   #print(c((forecast-(INP$Average_forecast-ny+1)*12+1):(forecast-(INP$Average_forecast-ny)*12)))
}

#rowMeans(pProdForMean)

# INP$p_Production[(forecast+1):(GLO$L_number+1), nc] = rep(as.numeric(as.character(INP$p_Production[(forecast-INP$Average_forecast*12+1):forecast, nc])), length(years.forecast) )
INP$p_Production[(forecast+1):(((simperiod+foreperiod)*12)+1), nc] = rep(as.numeric(as.character(rowMeans(pProdForMean))), length(years_forecast) )
                                        # GLO$L_number
}




# ----------------------------------------------------

if (IN_BEMTOOL) {

if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY) { 
        #eff_data <- read.csv(ALADYM_effort_path, sep=";")
        fishing_eff_result <- fact_calc(eff_data,"N")
        
        for (gear in 1:nb_gears) {
            for (i in 1:forecast){
            INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]         
            }
        }
 
 }
 }

# } 


if (as.numeric(INP$OPT_F_TYPE)==2) { 
Fm = INP$Fm    # AGGIUNTO
Fmales = Fm[as.character(Fm[,ncol(Fm)])=="M",]   # AGGIUNTO
Fmales=Fmales[,1:(ncol(Fmales)-1)]
Ffemales = Fm[as.character(Fm[,ncol(Fm)])=="F",]
Ffemales=Ffemales[,1:(ncol(Ffemales)-1)] 

} 



#-------------------------------------------------------------------------------
# Z estimated for M e F is the average on the last x years (as indicated by the user)
#-------------------------------------------------------------------------------
if (as.numeric(INP$OPT_F_TYPE)==1) {     # SOSTITUIRE CON CELLETTA NELLA GUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


index_final <- ifelse(!INTEGRATED_APPROACH, GLO$L_number, (simperiod+foreperiod)*12)

Z_da_mediare <- as.numeric(as.character(BAS$MZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
x <- seq_along(Z_da_mediare)
d1 <- split(Z_da_mediare, ceiling(x/12))
d2 <- data.frame(d1)
d2$avg <- rowMeans(d2)

BAS$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
INP$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )

Z_da_mediare <- as.numeric(as.character(BAS$FZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
x <- seq_along(Z_da_mediare)
d1 <- split(Z_da_mediare, ceiling(x/12))
d2 <- data.frame(d1)
d2$avg <- rowMeans(d2)

BAS$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
INP$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )

print("Total mortality M:", quote=F)
print(BAS$MZ_estimated[(forecast+1):(index_final+1)], quote=F )
print("Total mortality F:", quote=F)
print(BAS$FZ_estimated[(forecast+1):(index_final+1)], quote=F )


}




#-------------------------------------------------------------------------------
# Recruitment is the average on the last x years (as indicated by the user)
#-------------------------------------------------------------------------------    

# ------------------------------------------------------- SENZA incertezza sul reclutamento  
  if (! new_aldForecast@CI_recruitment) {
        # ------------------------------------------------------- SENZA incertezza sul reclutamento - reclutamento COSTANTE 
    if (new_aldForecast@recruitment_constant_or_SRR == 1) {   
          INP$FRLt_fore = 4
          INP$Recruits[(forecast+1):(GLO$L_number+1)] = as.numeric(as.character(new_aldForecast@recruitment.constant)) 
        # ------------------------------------------------------- SENZA incertezza sul reclutamento - SRR
    } else {
          INP$FRLt_fore                    <- which(SR_TYPE == data.frame(new_aldForecast@stockr.relationship)$relationship[1])
          
          INP$FRLa_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$a[1])
          INP$FRLb_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$b[1])
          INP$FRLc_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$c[1])
          
          INP$Recruits[(forecast+1):(GLO$L_number+1)] =  NA
 }
     # ------------------------------------------------------- CON incertezza sul reclutamento   
 } else {
        # ------------------------------------------------------- CON incertezza sul reclutamento - reclutamento COSTANTE   
    if (new_aldForecast@CI_recruitment_constant_or_SRR == 1) {
          INP$FRLt_fore <- 4
          INP$Recruits[(forecast+1):(GLO$L_number+1)] =  as.numeric(as.character(new_aldForecast@CI_recruitment_1_value)) 
          INP$Recruits_original <- INP$Recruits 
          INP$Recruits_original[(forecast+1):((simperiod+foreperiod)*12 +1)] <- as.numeric(as.character(new_aldForecast@CI_recruitment_1_value)) 
        # ------------------------------------------------------- CON incertezza sul reclutamento - SRR  
  } else {
          INP$FRLt_fore  <- which(SR_TYPE == new_aldForecast@CI_recruitment_2_SRR_model)          
          INP$Recruits[(forecast+1):(GLO$L_number+1)] =  NA
  }
 }  
 
INP$S_unit_fore                  <- which(SR_UNIT == new_aldForecast@stockr.unit)

if (!IN_BEMTOOL) {
if ( gtkToggleButtonGetActive(chkFMSY)) {
    Ref_point <-   as.numeric(as.character(new_aldForecast@target_F))           
    Ref_year <-   as.numeric(as.character(new_aldForecast@target_year) )
    Ref_month <- ( length(years) + which(years_forecast == Ref_year) ) * 12 

} else {
    Ref_point <- NA
    Ref_year <- NA
}
} else {
if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) {
    Ref_point <-   as.numeric(as.character(new_aldForecast@target_F))           
    Ref_year <-   as.numeric(as.character(new_aldForecast@target_year) )
	    Ref_month <- ( length(years) + which(years_forecast == Ref_year) ) * 12 

} else {
    Ref_point <- NA
    Ref_year <- NA
}
}




    
    
  if (!IN_BEMTOOL) {
    if (showCompTime)  {
 proc_ <- proc.time()
print(paste("Reading forecast input [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-runALADYMforecast_ptm[3]),2), "sec" ), quote=F )   
rm(runALADYMforecast_ptm)
}
  } else {
  if (!INTEGRATED_APPROACH) {
if (showCompTime)  {
 proc_ <- proc.time()
print(paste("Reading forecast input [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-runALADYMforecast_ptm[3]),2), "sec" ), quote=F )   
rm(runALADYMforecast_ptm)
} 
  }# else {
#if (showCompTime)  {
# proc_ <- proc.time()
#print(paste("Reading forecast input [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-runALADYMforecastINT_ptm[3]),2), "sec" ), quote=F )   
#rm(runALADYMforecastINT_ptm)
#}  
#  }
  }    
