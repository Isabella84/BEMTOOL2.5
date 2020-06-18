# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#
#
#
#
# Function that create a new fleet segment (SIMULATION) saving all the parameters from the GUI
#           
setFleetsegmentStatusQuo<-function(object_sim, object_fore) {

if (FALSE) {
object_sim = FleetList_simulation[[5]]
object_fore = FleetList_forecast[[5]]
}

object_fore@fleetname <- object_sim@fleetname 

if (new_aldSimulation@enteringMortality == "Z" ) {

object_fore@selectivity.mode  <-  object_sim@selectivity.mode

if (object_fore@selectivity.mode == "params") {

sel_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=8 ))
colnames(sel_fore_temp) <- c("year",	"month",	"param1",	"param2",	"param3",	"param4",	"param5",	"sel_type")

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 set selectivity
 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   sel_fore_temp$year <- years_rep
   sel_fore_temp$month <- months_rep

for (yea_f in 1:length(years_forecast)) {
    temp_sel <- data.frame(object_sim@selectivity.vector[(length(years)-1)*12+c(2:13), 2:ncol(sel_fore_temp)])
    colnames(temp_sel) <- colnames( sel_fore_temp[(yea_f-1)*12+c(1:12), 2:ncol(sel_fore_temp)])
     sel_fore_temp$month[(yea_f-1)*12+c(1:12)] <- as.character(temp_sel$month)
    sel_fore_temp[(yea_f-1)*12+c(1:12), 3:ncol(sel_fore_temp)] <- temp_sel[,2:ncol(temp_sel)]
}
object_fore@selectivity.vector  <- sel_fore_temp

}  else if (object_fore@selectivity.mode == "age") {

    dataframe_sel_age_M <- data.frame(matrix(nrow=length(years_forecast), ncol=(n_ages_M + 1)))
 colnames(dataframe_sel_age_M) <-  c( "Year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
  dataframe_sel_age_F <- data.frame(matrix(nrow=length(years_forecast), ncol=(n_ages_F +1)))
  colnames(dataframe_sel_age_F) <-  c( "Year",  paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
  
    dataframe_sel_age_M$Year <- years_forecast
    dataframe_sel_age_F$Year <- years_forecast
    
    dataframe_sel_age_F[, c(2:ncol(dataframe_sel_age_F))] <-  as.numeric(as.character(object_sim@SelectivityAge.F.vector[length(years), c(2:ncol(dataframe_sel_age_F))]  ))
   dataframe_sel_age_M[, c(2:ncol(dataframe_sel_age_M))] <-  as.numeric(as.character(object_sim@SelectivityAge.M.vector[length(years), c(2:ncol(dataframe_sel_age_M))]  ))
   
  object_fore@SelectivityAge.F.vector <- dataframe_sel_age_F
  object_fore@SelectivityAge.M.vector  <- dataframe_sel_age_M
}

}


# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 END set selectivity

  object_fore@production.datatype  <- object_sim@production.datatype  

  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 set discard
object_fore@discard.calculation <- object_sim@discard.calculation
object_fore@discard.datatype <- object_sim@discard.datatype

if (object_sim@discard.calculation == "YES") {
if (nrow(object_sim@discard.vector) > 0) {
dis_ogive_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)*12), ncol=4))
colnames(dis_ogive_fore_temp) <- c("year",	"month",	"L50", "L75_L25")

 years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   months_rep <- rep(MONTHS, length(years_forecast))
   dis_ogive_fore_temp$year <- years_rep
   dis_ogive_fore_temp$month <- months_rep

for (yea_f in 1:length(years_forecast)) {
    temp_dis <- data.frame(object_sim@discard.vector[(length(years)-1)*12+c(2:13), 2:ncol(dis_ogive_fore_temp)])
    colnames(temp_dis) <- colnames( dis_ogive_fore_temp[(yea_f-1)*12+c(1:12), 2:ncol(dis_ogive_fore_temp)])
     dis_ogive_fore_temp$month[(yea_f-1)*12+c(1:12)] <- as.character(temp_dis$month)
    dis_ogive_fore_temp[(yea_f-1)*12+c(1:12), 3:ncol(dis_ogive_fore_temp)] <- temp_dis[,2:ncol(temp_dis)]
}
object_fore@discard.vector  <- dis_ogive_fore_temp
#print(object_fore@discard.vector)
}

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 END discard


  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 set discard external vector
if (nrow(object_sim@discard_extvector.F.vector ) > 0) {
nAGES_F <- ncol(object_sim@discard_extvector.F.vector)-1
dis_extF_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)), ncol=nAGES_F+1))
colnames(dis_extF_fore_temp) <- c("year",	colnames(object_sim@discard_extvector.F.vector)[2:(nAGES_F+1)])
dis_extF_fore_temp$year <- years_forecast
                                                 
for (yea_f in 1:length(years_forecast)) {
    dis_extF_fore_temp[yea_f, 2:(nAGES_F+1)] <- as.numeric(as.character(object_sim@discard_extvector.F.vector[length(years), 2:(nAGES_F+1)]))  
}
object_fore@discard_extvector.F.vector  <- dis_extF_fore_temp
}

if (nrow(object_sim@discard_extvector.M.vector ) > 0) {
nAGES_M <- ncol(object_sim@discard_extvector.M.vector)-1
dis_extM_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)), ncol=nAGES_M+1))
colnames(dis_extM_fore_temp) <-c("year",	colnames(object_sim@discard_extvector.M.vector)[2:(nAGES_M+1)])
dis_extM_fore_temp$year <- years_forecast
                                                 
for (yea_f in 1:length(years_forecast)) {
    dis_extM_fore_temp[yea_f, 2:(nAGES_M+1)] <- as.numeric(as.character(object_sim@discard_extvector.M.vector[length(years), 2:(nAGES_M+1)]))  
}
object_fore@discard_extvector.M.vector  <- dis_extM_fore_temp
}

}

  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 set landing obligation
if (nrow(object_sim@landing.obligation.vector ) > 0) {
LAND_OBL_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)), ncol=13))
colnames(LAND_OBL_fore_temp) <- c("year",	MONTHS)
LAND_OBL_fore_temp$year <- years_forecast
                                                 
for (yea_f in 1:length(years_forecast)) {
    LAND_OBL_fore_temp[2:13] <- as.character(object_sim@landing.obligation.vector[length(years), 2:13])  
}
object_fore@landing.obligation.vector  <- LAND_OBL_fore_temp
}
  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 end landing obligation
  
  
  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 set EFFORT 
  
  object_fore@effort.datatype  <- object_sim@effort.datatype

  if (object_fore@effort.datatype != "Fishing coefficient" ) {

  mat_fore_temp_2 <- data.frame(matrix(nrow=(length(years_forecast)), ncol=13))
colnames(mat_fore_temp_2) <- c("year",	MONTHS)
mat_fore_temp_2$year <- years_forecast

  mat_fore_temp_3 <- data.frame(matrix(nrow=(length(years_forecast)), ncol=13))
colnames(mat_fore_temp_3) <- c("year",	MONTHS)
mat_fore_temp_3$year <- years_forecast

  mat_fore_temp_4 <- data.frame(matrix(nrow=(length(years_forecast)), ncol=13))
colnames(mat_fore_temp_4) <- c("year",	MONTHS)
mat_fore_temp_4$year <- years_forecast
                                                 
for (yea_f in 1:length(years_forecast)) {
    mat_fore_temp_2[yea_f, 2:13] <- as.numeric(as.character(object_sim@vessels.vector[length(years), 3:14]))   
    mat_fore_temp_3[yea_f, 2:13] <- as.numeric(as.character(object_sim@days.vector[length(years), 3:14]))  
    mat_fore_temp_4[yea_f, 2:13] <- as.numeric(as.character(object_sim@gt.vector[length(years), 3:14]))    
}

object_fore@vessels.vector  <-  mat_fore_temp_2
object_fore@days.vector  <-  mat_fore_temp_3
object_fore@gt.vector  <-  mat_fore_temp_4

} else {

  mat_fore_temp <- data.frame(matrix(nrow=(length(years_forecast)), ncol=13))
colnames(mat_fore_temp) <- c("year",	MONTHS)
mat_fore_temp$year <- years_forecast
                                                 
for (yea_f in 1:length(years_forecast)) {
    mat_fore_temp[yea_f, 2:13] <- as.numeric(as.character(object_sim@fishingeffort.vector[length(years), 3:14]))    
}
object_fore@fishingeffort.vector  <- mat_fore_temp

}

object_fore@scenario.reduction <- SCENARIO_TYPE[1]

  # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 end landing obligation

 object_fore@discard.survivability.calculation <- object_sim@discard.survivability.calculation 
  object_fore@discard.survivability.datatype <- object_sim@discard.survivability.datatype  
 object_fore@discard.survivability.params <- object_sim@discard.survivability.params
 object_fore@escape.survivability.calculation <- object_sim@escape.survivability.calculation 
 object_fore@escape.survivability.datatype <- object_sim@escape.survivability.datatype
 object_fore@escape.survivability.constant <- object_sim@escape.survivability.constant
 object_fore@escape.survivability.DOS.datatype  <- object_sim@escape.survivability.DOS.datatype
 object_fore@escape.survivability.DOS.ogiveparams <-  object_sim@escape.survivability.DOS.ogiveparams
 object_fore@escape.survivability.DOS.ext_vect.M  <- object_sim@escape.survivability.DOS.ext_vect.M
 object_fore@escape.survivability.DOS.ext_vect.F <-  object_sim@escape.survivability.DOS.ext_vect.F
 


#print("DOPO L'AGGIORNAMENTO!", quote=F)
#try(  print(FleetList_simulation[[2]]@fishingmortality.M.vector)  )

return(object_fore)
}
