# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# #############################################################################################
# SET ALADYM FROM BEMTOOL IN ACCORDING TO THE SCENARIO
# #############################################################################################

get_production_data<-function() {

  nb_years <- length(years)
 
prod_data_all <- data.frame(matrix(nrow=0, ncol=4))
colnames(prod_data_all) <- c("Gear",	"Month",	"Production",	"Unit")


for (n_prod in 1:length(FLEETSEGMENTS_names)) {
   prod_data_fleet <- data.frame(matrix(nrow=0, ncol=4))
colnames(prod_data_fleet) <- c("Gear",	"Month",	"Production",	"Unit")

for (yy in 1:nb_years) {
    prod_data_temp <- as.numeric( as.character(FleetList_simulation[[n_prod]]@production.vector[yy, 3:14] ))
    if (yy==1) {
    to_add <- data.frame(matrix(c(FLEETSEGMENTS_names[n_prod],0, FleetList_simulation[[n_prod]]@production.vector[1,2], "kg"), nrow=1))
        colnames(to_add) <- c("Gear",	"Month",	"Production",	"Unit")
    prod_data_fleet <- rbind(prod_data_fleet,  to_add)
    }
    to_add <- data.frame(cbind(rep(FLEETSEGMENTS_names[n_prod], 12), c((yy-1)*12 + c(1:12)), prod_data_temp, "kg") )
    colnames(to_add) <- c("Gear",	"Month",	"Production",	"Unit")
    prod_data_fleet <- rbind(prod_data_fleet, to_add)
} # end years
 

prod_data_all <- rbind(prod_data_all, prod_data_fleet)

}  # end fleet loop 
 
 
#if (phase == "SIMULATION") {
#write.table(prod_data_all, paste(ALADYM_home, "/BEMTOOL_automatic_Production_data_", BMT_SPECIES[ALADYM_spe],"_SIM.csv", sep=""), sep=";", row.names=F)                  
#} else {
#write.table(prod_data_all, paste(ALADYM_home, "/BEMTOOL_automatic_Production_data_", BMT_SPECIES[ALADYM_spe],"_FORE.csv", sep=""), sep=";", row.names=F)                  
#}

# if (phase=="SIMULATION") {
#       save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\[", casestudy_name, "] Production data SIM.csv", sep="")
# } else {
# if (!MEY_CALCULATION) {
#       save_path <- paste(casestudy_path, "\\", harvest_rule_id,"\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\[", casestudy_name, "] Production data FORE ", harvest_rule_id,".csv", sep="")
#} else {
#       save_path <- paste(casestudy_path, "\\MEY calculation\\", harvest_rule_id,"\\ALADYM\\", BMT_SPECIES[ALADYM_spe],"\\[", casestudy_name, "] Production data FORE ", harvest_rule_id,".csv", sep="")
#}
# } 


prod_data_all_temp <- prod_data_all
colnames(prod_data_all_temp) <- c("fleet_segment",	"Month",	"PRODUCTION",	"Unit")

write.table(prod_data_all_temp, file=PRODUCTION_DATA_working_table, sep=";", row.names=F)

 
return(prod_data_all) 
}
