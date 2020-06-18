# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





get_table<-function(tbl) {
return_table <- NULL
if (tbl == "RECRUITMENT") {
recruitment_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(recruitment_table) <- c("year","seed", MONTHS)
recruitment_table$year <- years
recruitment_table$seed <- ""
recruitment_table$seed[1] <- gtkEntryGetText(entry_OFFSPRING_seedvalue)     
for (i in 1:length(recruitments)) {
for (m in 3:14) { recruitment_table[as.character(recruitment_table$year) == as.character(recruitments[[i]]$year),m] <-  recruitments[[i]][m-1] }
}
return_table <- recruitment_table

} else if (tbl == "NATURAL_MORTALITY_M")  {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

table_MM <- data.frame(matrix(0, nrow = length(c(Tr:(n_ages*12))), ncol=2)) 
heading <- c("age_month", "M")
   colnames(table_MM) <- heading
   table_MM$age_month <- as.character(c(Tr:(n_ages*12)))
           
for (i in 1:length(Mvector_M)) {
      table_MM$M[i] <-  Mvector_M[[i]]$M
} 
return_table <- table_MM

} else if (tbl == "NATURAL_MORTALITY_F")  {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

table_MF <- data.frame(matrix(0, nrow = length(c(Tr:(n_ages*12))), ncol=2)) 
heading <- c("age_month", "M")
   colnames(table_MF) <- heading
   table_MF$age_month <- as.character(c(Tr:(n_ages*12)))
           
for (i in 1:length(Mvector_F)) {
      table_MF$M[i] <-  Mvector_F[[i]]$M
} 
return_table <- table_MF

} else if (tbl == "TOTAL_MORTALITY_M")  {
Zvector_M_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(Zvector_M_table) <- c("year","seed", MONTHS)
Zvector_M_table$year <- years
Zvector_M_table$seed <- ""
Zvector_M_table$seed[1] <- gtkEntryGetText(entry_Zseedvalue_M)
     
for (i in 1:length(Zvector_M)) {
  for (m in 3:14) {
      Zvector_M_table[as.character(Zvector_M_table$year) == as.character(Zvector_M[[i]]$year),m] <-  Zvector_M[[i]][m-1]
  }
} 
return_table <- Zvector_M_table

} else if (tbl == "TOTAL_MORTALITY_F")  {
Zvector_F_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(Zvector_F_table) <- c("year","seed", MONTHS)
Zvector_F_table$year <- years
Zvector_F_table$seed <- ""
Zvector_F_table$seed[1] <- gtkEntryGetText(entry_Zseedvalue_F)
     
for (i in 1:length(Zvector_F)) {
  for (m in 3:14) {
      Zvector_F_table[as.character(Zvector_F_table$year) == as.character(Zvector_F[[i]]$year),m] <-  Zvector_F[[i]][m-1]
  }
} 
return_table <- Zvector_F_table

} else if (tbl == "SELECTIVITY") {
selectivity_table <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=(selectivity_params$n_par +2) ))
 if (selectivity_params$n_par == 2) {
   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
 } else if (selectivity_params$n_par == 3) {
   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
 } else if (selectivity_params$n_par == 5) {
   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
 } 
colnames(selectivity_table) <- heading

 years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   selectivity_table$year <- years_rep
   selectivity_table$month <- months_rep
 
 if (length(selectivities) != 0) {
for (i in 1:length(selectivities)) {
  for (m in 1:((length(years)*12)+1)) {
      selectivity_table[m, 3] <-  as.double(selectivities[[m]][3])
      selectivity_table[m, 4] <-  as.double(selectivities[[m]][4])
     
     if (selectivity_params$n_par > 2) {
          selectivity_table[m, 5] <-  as.double(selectivities[[m]][5])
      }
     if (selectivity_params$n_par > 4) {
          selectivity_table[m, 6] <-  as.double(selectivities[[m]][6])
          selectivity_table[m, 7] <-  as.double(selectivities[[m]][7])
 }  
      
  }
}

} 
return_table <- selectivity_table 

} else if (tbl=="PRODUCTION") {
 production_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(production_table) <- c("year","seed", MONTHS)
production_table$year <- years
production_table$seed <- ""
production_table$seed[1] <- gtkEntryGetText(entry_Production_seedvalue)
     
for (i in 1:length(productions)) {
  for (m in 3:14) {
      production_table[as.character(production_table$year) == as.character(productions[[i]]$year),m] <-  productions[[i]][m-1]
  }
} 
return_table <- production_table

} else if (tbl=="P_PRODUCTION") {
 pproduction_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(pproduction_table) <- c("year","seed", MONTHS)
pproduction_table$year <- years
pproduction_table$seed <- ""
pproduction_table$seed[1] <- gtkEntryGetText(entry_pProduction_seedvalue)
     
for (i in 1:length(pproductions)) {
  for (m in 3:14) {
      pproduction_table[as.character(pproduction_table$year) == as.character(pproductions[[i]]$year),m] <-  pproductions[[i]][m-1]
  }
} 
return_table <- pproduction_table

} else if (tbl =="DISCARD") {
discard_table <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=4 ))
colnames(discard_table) <- c("year","month", "L50", "L75_L25")

 years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   discard_table$year <- years_rep
   discard_table$month <- months_rep
 
for (i in 1:length(discards_list)) {
  for (m in 1:((length(years)*12)+1)) {
      discard_table$L50[m] <-  as.double(discards_list[[m]][3])
      discard_table$L75_L25[m] <-  as.double(discards_list[[m]][4])
  }
} 
return_table <- discard_table

} else if (tbl == "FISHING_EFFORT") {

FISHINGEFFORT_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(FISHINGEFFORT_table) <- c("year","seed", MONTHS)
FISHINGEFFORT_table$year <- years
FISHINGEFFORT_table$seed <- ""
FISHINGEFFORT_table$seed[1] <- gtkEntryGetText(entry_FISHINGEFFORT_seedvalue)
     
for (i in 1:length(FISHINGEFFORT)) {
  for (m in 3:14) {
      FISHINGEFFORT_table[as.character(FISHINGEFFORT_table$year) == as.character(FISHINGEFFORT[[i]]$year),m] <-  FISHINGEFFORT[[i]][m-1]
  }
} 
return_table <- FISHINGEFFORT_table

} else if (tbl == "VESSELS") {

VESSELS_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(VESSELS_table) <- c("year","seed", MONTHS)
VESSELS_table$year <- years
VESSELS_table$seed <- ""
VESSELS_table$seed[1] <- gtkEntryGetText(entry_VESSELS_seedvalue)
     
for (i in 1:length(VESSELS)) {
  for (m in 3:14) {
      VESSELS_table[as.character(VESSELS_table$year) == as.character(VESSELS[[i]]$year),m] <-  VESSELS[[i]][m-1]
  }
} 
return_table <- VESSELS_table

} else if (tbl == "DAYS") {

DAYS_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(DAYS_table) <- c("year","seed", MONTHS)
DAYS_table$year <- years
DAYS_table$seed <- ""
DAYS_table$seed[1] <- gtkEntryGetText(entry_DAYS_seedvalue)
     
for (i in 1:length(DAYS)) {
  for (m in 3:14) {
      DAYS_table[as.character(DAYS_table$year) == as.character(DAYS[[i]]$year),m] <-  DAYS[[i]][m-1]
  }
} 
return_table <- DAYS_table

} else if (tbl == "GT") {

GT_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(GT_table) <- c("year","seed", MONTHS)
GT_table$year <- years
GT_table$seed <- ""
GT_table$seed[1] <- gtkEntryGetText(entry_GT_seedvalue)
     
for (i in 1:length(GT)) {
  for (m in 3:14) {
      GT_table[as.character(GT_table$year) == as.character(GT[[i]]$year),m] <-  GT[[i]][m-1]
  }
} 
return_table <- GT_table

} else if (tbl == "SELECTIVITY_FORE") {

selectivity_fore_table <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=(selectivity_params$n_par +2) ))
 if (selectivity_params$n_par == 2) {
   heading <- c("year","month",  as.character(selectivity_params$param1),  as.character(selectivity_params$param2))
 } else if (selectivity_params$n_par == 3) {
   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3))
 } else if (selectivity_params$n_par == 5) {
   heading <- c("year","month",   as.character(selectivity_params$param1),  as.character(selectivity_params$param2),  as.character(selectivity_params$param3),  as.character(selectivity_params$param4),  as.character(selectivity_params$param5) )
 } 
colnames(selectivity_fore_table) <- heading

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   selectivity_fore_table$year <- years_rep
   selectivity_fore_table$month <- months_rep
 
for (i in 1:length(selectivities_fore)) {
  for (m in 1:((length(years_forecast)*12))) {
      selectivity_fore_table[m, 3] <-  as.double(selectivities_fore[[m]][3])
      selectivity_fore_table[m, 4] <-  as.double(selectivities_fore[[m]][4])
     
     if (selectivity_params$n_par > 2) {
          selectivity_fore_table[m, 5] <-  as.double(selectivities_fore[[m]][5])
      }
     if (selectivity_params$n_par > 4) {
          selectivity_fore_table[m, 6] <-  as.double(selectivities_fore[[m]][6])
          selectivity_fore_table[m, 7] <-  as.double(selectivities_fore[[m]][7])
 }  
      
  }
}
return_table <- selectivity_fore_table 

} else if (tbl =="DISCARD_FORE") {
discard_fore_table <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=4 ))
colnames(discard_fore_table) <- c("year","month", "L50", "L75_L25")

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   discard_fore_table$year <- years_rep
   discard_fore_table$month <- months_rep
 
for (i in 1:length(discards_fore)) {
  for (m in 1:((length(years_forecast)*12))) {
      discard_fore_table$L50[m] <-  as.double(discards_fore[[m]][3])
      discard_fore_table$L75_L25[m] <-  as.double(discards_fore[[m]][4])
  }
} 
return_table <- discard_fore_table

} else if (tbl == "FISHING_EFFORT_FORE") {

FISHINGEFFORT_table_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(length(MONTHS)+1) ))
colnames(FISHINGEFFORT_table_fore) <- c("year", MONTHS)
FISHINGEFFORT_table_fore$year <- years_forecast
     
for (i in 1:length(FISHINGEFFORT_fore)) {
  for (m in 2:13) {
      FISHINGEFFORT_table_fore[as.character(FISHINGEFFORT_table_fore$year) == as.character(FISHINGEFFORT_fore[[i]]$year),m] <-  FISHINGEFFORT_fore[[i]][m]
  }
} 
return_table <- FISHINGEFFORT_table_fore

} else if (tbl == "VESSELS_FORE") {

VESSELS_table_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(length(MONTHS)+1) ))
colnames(VESSELS_table_fore) <- c("year", MONTHS)
VESSELS_table_fore$year <- years_forecast
     
for (i in 1:length(VESSELS_fore)) {
  for (m in 2:13) {
      VESSELS_table_fore[as.character(VESSELS_table_fore$year) == as.character(VESSELS_fore[[i]]$year),m] <-  VESSELS_fore[[i]][m]
  }
} 
return_table <- VESSELS_table_fore

} else if (tbl == "DAYS_FORE") {

DAYS_table_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(length(MONTHS)+1) ))
colnames(DAYS_table_fore) <- c("year", MONTHS)
DAYS_table_fore$year <- years_forecast
     
for (i in 1:length(DAYS_fore)) {
  for (m in 2:13) {
      DAYS_table_fore[as.character(DAYS_table_fore$year) == as.character(DAYS_fore[[i]]$year),m] <-  DAYS_fore[[i]][m]
  }
} 
return_table <- DAYS_table_fore

} else if (tbl == "GT_FORE") {

GT_table_fore <- data.frame(matrix(nrow=length(years_forecast), ncol=(length(MONTHS)+1) ))
colnames(GT_table_fore) <- c("year", MONTHS)
GT_table_fore$year <- years_forecast
     
for (i in 1:length(GT_fore)) {
  for (m in 2:13) {
      GT_table_fore[as.character(GT_table_fore$year) == as.character(GT_fore[[i]]$year),m] <-  GT_fore[[i]][m]
  }
} 
return_table <- GT_table_fore

} else if (tbl == "FISHING_MORTALITY_M")  {
n_ages_mal <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))
first_age_mal <- 0

    n_ages_mal <- n_ages_mal - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

#print(paste("years for fishing mortality table (MALES):", n_ages_mal))
FishingMvector_M_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_mal+1) ))
colnames(FishingMvector_M_table) <- c("year",paste("age", c(first_age_mal:(n_ages_mal+first_age_mal-1)), sep=""))
FishingMvector_M_table$year <- years
 
if (length(FishingMvector_M) != 0) {
names_ages <-  colnames(FishingMvector_M_table[ which(colnames(FishingMvector_M_table) %in% names(FishingMvector_M[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]
for (i in 1:length(FishingMvector_M)) {
 for (m in names_ages) {
FishingMvector_M_table[as.character(FishingMvector_M_table$year) == as.character(FishingMvector_M[[i]]$year),colnames(FishingMvector_M_table) == m] <-  FishingMvector_M[[i]][m] 
 }
}
}
 
return_table <- FishingMvector_M_table

} else if (tbl == "FISHING_MORTALITY_F")  {
n_ages_fem <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))
first_age_fem <- 0

    n_ages_fem <- n_ages_fem - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

FishingMvector_F_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_fem+1) ))
colnames(FishingMvector_F_table) <-  c("year",paste("age", c(first_age_fem:(n_ages_fem+first_age_fem-1)), sep=""))
FishingMvector_F_table$year <- years

if (length(FishingMvector_F) != 0) {
 names_ages <-  colnames(FishingMvector_F_table[ which(colnames(FishingMvector_F_table) %in% names(FishingMvector_F[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]

for (i in 1:length(FishingMvector_F)) {
  for (m in names_ages) {
FishingMvector_F_table[as.character(FishingMvector_F_table$year) == as.character(FishingMvector_F[[i]]$year),colnames(FishingMvector_F_table) == m] <-  FishingMvector_F[[i]][m] 
  }
}
}
      
return_table <- FishingMvector_F_table
# print(FishingMvector_F_table)

} else if (tbl == "DISCARD_EXT_VECTOR_F")  {
n_ages_fem <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))
first_age_fem <- 0

    n_ages_fem <- n_ages_fem - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

discards_extvector_F_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_fem+1) ))
colnames(discards_extvector_F_table) <-  c("year",paste("age", c(first_age_fem:(n_ages_fem+first_age_fem-1)), sep=""))
discards_extvector_F_table$year <- years

if (length(discards_extvector_F_list) != 0) {
 names_ages <-  colnames(discards_extvector_F_table[ which(colnames(discards_extvector_F_table) %in% names(discards_extvector_F_list[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]

for (i in 1:length(discards_extvector_F_list)) {
  for (m in names_ages) {
discards_extvector_F_table[as.character(discards_extvector_F_table$year) == as.character(discards_extvector_F_list[[i]]$year),colnames(discards_extvector_F_table) == m] <-  discards_extvector_F_list[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_F_table
# print(FishingMvector_F_table)

} else if (tbl == "DISCARD_EXT_VECTOR_M")  {
n_ages_mal <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))
first_age_mal <- 0

    n_ages_mal <- n_ages_mal - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

discards_extvector_M_table <- data.frame(matrix(nrow=length(years), ncol=(n_ages_mal+1) ))
colnames(discards_extvector_M_table) <-  c("year",paste("age", c(first_age_mal:(n_ages_mal+first_age_mal-1)), sep=""))
discards_extvector_M_table$year <- years

if (length(discards_extvector_M_list) != 0) {
 names_ages <-  colnames(discards_extvector_M_table[ which(colnames(discards_extvector_M_table) %in% names(discards_extvector_M_list[[1]]) ) ] )
names_ages <- names_ages[2:length(names_ages)]

for (i in 1:length(discards_extvector_M_list)) {
  for (m in names_ages) {
discards_extvector_M_table[as.character(discards_extvector_M_table$year) == as.character(discards_extvector_M_list[[i]]$year),colnames(discards_extvector_M_table) == m] <-  discards_extvector_M_list[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_M_table
# print(FishingMvector_F_table)
} else if (tbl == "LANDING_OBLIGATION") {

LAND_OBL_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(LAND_OBL_table) <- c("year","seed", MONTHS)
LAND_OBL_table$year <- years
LAND_OBL_table$seed <- ""
LAND_OBL_table$seed[1] <- gtkEntryGetText(entry_lan_obligation_seedvalue)
     
for (i in 1:length(lan_obligation)) {
  for (m in 3:14) {
      LAND_OBL_table[as.character(LAND_OBL_table$year) == as.character(lan_obligation[[i]]$year),m] <-  lan_obligation[[i]][m-1]
  }
} 
return_table <- LAND_OBL_table

} else if (tbl == "ESCAPE_SURVIVABILITY_EXT_VECTOR_F")  {
n_ages_fem <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))
first_age_fem <- 0

    n_ages_fem <- n_ages_fem - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

discards_extvector_F_table <- data.frame(matrix(nrow=1, ncol=(n_ages_fem) ))
colnames(discards_extvector_F_table) <-  c(paste("age", c(first_age_fem:(n_ages_fem+first_age_fem-1)), sep=""))

if (length(escape_survival_extvector_F_list) != 0) {
 names_ages <-  colnames(discards_extvector_F_table[ which(colnames(discards_extvector_F_table) %in% names(escape_survival_extvector_F_list[[1]]) ) ] )
names_ages <- names_ages

for (i in 1:length(escape_survival_extvector_F_list)) {
  for (m in names_ages) {
discards_extvector_F_table[1,colnames(discards_extvector_F_table) == m] <-  escape_survival_extvector_F_list[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_F_table
# print(FishingMvector_F_table)

} else if (tbl == "ESCAPE_SURVIVABILITY_EXT_VECTOR_M")  {
n_ages_mal <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))
first_age_mal <- 0

    n_ages_mal <- n_ages_mal - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

discards_extvector_M_table <- data.frame(matrix(nrow=1, ncol=(n_ages_mal) ))
colnames(discards_extvector_M_table) <-  c(paste("age", c(first_age_mal:(n_ages_mal+first_age_mal-1)), sep=""))

if (length(escape_survival_extvector_M_list) != 0) {
 names_ages <-  colnames(discards_extvector_M_table[ which(colnames(discards_extvector_M_table) %in% names(escape_survival_extvector_M_list[[1]]) ) ] )
names_ages <- names_ages

for (i in 1:length(escape_survival_extvector_M_list)) {
  for (m in names_ages) {
discards_extvector_M_table[1,colnames(discards_extvector_M_table) == m] <-  escape_survival_extvector_M_list[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_M_table
# print(FishingMvector_F_table)

} else if (tbl == "ESCAPE_SURVIVABILITY_EXT_VECTOR_F_FORE")  {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages_fem <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages_fem <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

first_age_fem <- 0
    n_ages_fem <- n_ages_fem - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

discards_extvector_F_table <- data.frame(matrix(nrow=1, ncol=(n_ages_fem) ))
colnames(discards_extvector_F_table) <-  c(paste("age", c(first_age_fem:(n_ages_fem+first_age_fem-1)), sep=""))

if (length(escape_survival_extvector_F_list_fore) != 0) {
 names_ages <-  colnames(discards_extvector_F_table[ which(colnames(discards_extvector_F_table) %in% names(escape_survival_extvector_F_list_fore[[1]]) ) ] )
names_ages <- names_ages

for (i in 1:length(escape_survival_extvector_F_list_fore)) {
  for (m in names_ages) {
discards_extvector_F_table[1,colnames(discards_extvector_F_table) == m] <-  escape_survival_extvector_F_list_fore[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_F_table
# print(FishingMvector_F_table)

} else if (tbl == "ESCAPE_SURVIVABILITY_EXT_VECTOR_M_FORE")  {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages_mal <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages_mal <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

first_age_mal <- 0
    n_ages_mal <- n_ages_mal - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
    
discards_extvector_M_table <- data.frame(matrix(nrow=1, ncol=(n_ages_mal) ))
colnames(discards_extvector_M_table) <-  c(paste("age", c(first_age_mal:(n_ages_mal+first_age_mal-1)), sep=""))

if (length(escape_survival_extvector_M_list_fore) != 0) {
 names_ages <-  colnames(discards_extvector_M_table[ which(colnames(discards_extvector_M_table) %in% names(escape_survival_extvector_M_list_fore[[1]]) ) ] )
names_ages <- names_ages

for (i in 1:length(escape_survival_extvector_M_list_fore)) {
  for (m in names_ages) {
discards_extvector_M_table[1,colnames(discards_extvector_M_table) == m] <-  escape_survival_extvector_M_list_fore[[i]][m] 
  }
}
}
      
return_table <- discards_extvector_M_table
# print(FishingMvector_F_table)

} 

#print(paste("Returned table:", tbl), quote=F)

return(return_table)
}
 