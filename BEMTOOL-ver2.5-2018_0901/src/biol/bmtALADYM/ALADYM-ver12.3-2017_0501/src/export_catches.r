# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




export_catches <- function(End){


if (showCompTime)  {
export_catches_ptm <- proc.time()  
}

if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
print(FLEETSEGMENTS_names)
}


loca_xa <- 1:( End/ INP$Time_slice)


Catch_temp =  loca_xa
Catch_temp = cbind(Catch_temp,sumWequals(SRO$Biological_production[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice))
Catch_temp = cbind(Catch_temp,sumWequals(SRO$Death_biomass[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice))
Catch_temp = cbind(Catch_temp,sumWequals(SRO$Capture_biomass[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice))

if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Catch_temp = cbind(Catch_temp,sumWequals(SRO$Capture_biomass_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Catch_temp = cbind(Catch_temp,meanWequals(SRO$Capture_length_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Catch_temp = cbind(Catch_temp,meanWequals(SRO$Capture_length_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Catch_temp = cbind(Catch_temp,meanWequals(SRO$Capture_age_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Catch_temp = cbind(Catch_temp,meanWequals(SRO$Capture_age_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}



if(length(FLEETSEGMENTS_names)==1) {
colnames (Catch_temp) = c("Year","Biological_Production","Death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch")
}  else {
colnames (Catch_temp) = c("Year","Biological_Production","Death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names,sep=""))
}
Catch_temp=data.frame(Catch_temp)
write.table(Catch_temp, CATCH_table,row.names=FALSE, sep=";")

# landing
Landing_temp =  loca_xa
Landing_temp = cbind(Landing_temp,sumWequals(SRO$Landing_biomass[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice))

if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Landing_temp = cbind(Landing_temp,sumWequals(SRO$Landing_biomass_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Landing_temp = cbind(Landing_temp,meanWequals(SRO$Landing_length_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Landing_temp = cbind(Landing_temp,meanWequals(SRO$Landing_length_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Landing_temp = cbind(Landing_temp,meanWequals(SRO$Landing_age_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Landing_temp = cbind(Landing_temp,meanWequals(SRO$Landing_age_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}

if(length(FLEETSEGMENTS_names)==1) {
colnames (Landing_temp) = c("Year","Total_Landing","Mean_length_in_landing","Mean_age_in_landing")
}  else {
colnames (Landing_temp) = c("Year","Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_landing",paste("Mean_length_in_landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_landing",paste("Mean_age_in_landing_",FLEETSEGMENTS_names,sep=""))
}
Landing_temp=data.frame(Landing_temp)
write.table(Landing_temp, LANDING_table,row.names=FALSE, sep=";")


#-----
#discard
Discard_temp =  loca_xa
Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass[1:(GLO$L_number + 1)] , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean[1:(GLO$L_number + 1)]        , GLO$L_number + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears[1:(GLO$L_number + 1),g] , GLO$L_number + 1, INP$Time_slice))
  }
}


#for (g in 1:length(FLEETSEGMENTS_names)) {
#Discard_temp = cbind(Discard_temp,Discard_temp[,g+2]/Landing_temp[,g+2])
#}  
#
if(length(FLEETSEGMENTS_names)==1) {
colnames (Discard_temp) = c("Year","Total_Discard","Mean_length_in_discard","Mean_age_in_discard")
}  else {
colnames (Discard_temp) = c("Year","Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_discard",paste("Mean_length_in_discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_discard",paste("Mean_age_in_discard_",FLEETSEGMENTS_names,sep=""))
}
Discard_temp=data.frame(Discard_temp)
#Discard_temp$Discard_ratio = Discard_temp$Total_Discard/Landing_temp$Total_Landing
write.table(Discard_temp, DISCARD_table,row.names=FALSE, sep=";")


 if (showCompTime)  {
 proc_ <- proc.time()
# SIMULATION_EXPLOITED_ptm <- proc.time()
print(paste("export_catches [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-export_catches_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - SIMULATION_EXPLOITED_ptm, quote=F ) 
rm(export_catches_ptm)
}


}
