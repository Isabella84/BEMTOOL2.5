# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# #############################################################################################
# SET ALADYM FROM BEMTOOL IN ACCORDING TO THE SCENARIO
# #############################################################################################

get_production_data<-function() {

if (phase == "SIMULATION") {
nb_years <- simperiod
 } else {
  if (!INTEGRATED_APPROACH) {
  nb_years <- simperiod + foreperiod
  } else {
  nb_years <- simperiod + current_year
  }
 }
 
# table of Production data  (Gear	Month	Production	Unit)
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)


prod_data_all <- data.frame(matrix(nrow=0, ncol=4))
colnames(prod_data_all) <- c("Gear",	"Month",	"Production",	"Unit")

   fleet_interaction_ord <- 1
for (n_prod in 1:n_fleet) {
   prod_data_fleet <- data.frame(matrix(nrow=0, ncol=4))
colnames(prod_data_fleet) <- c("Gear",	"Month",	"Production",	"Unit")

  if (n_prod %in% associated_fleetsegment_indices)  {
for (yy in 1:nb_years) {
    prod_data_temp <- as.numeric( as.character(Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$historicalLandings) )
    if (yy==1) {
    to_add <- data.frame(matrix(c(BMT_FLEETSEGMENTS[n_prod],0, prod_data_temp[1], "kg"), nrow=1))
        colnames(to_add) <- c("Gear",	"Month",	"Production",	"Unit")
    prod_data_fleet <- rbind(prod_data_fleet,  to_add)
    }
    to_add <- data.frame(cbind(rep(BMT_FLEETSEGMENTS[n_prod], 12), c((yy-1)*12 + c(1:12)), prod_data_temp, "kg") )
    colnames(to_add) <- c("Gear",	"Month",	"Production",	"Unit")
    prod_data_fleet <- rbind(prod_data_fleet, to_add)
} # end years
    fleet_interaction_ord <- fleet_interaction_ord +1
} 

prod_data_all <- rbind(prod_data_all, prod_data_fleet)

}  # end fleet loop 
 
 
#if (phase == "SIMULATION") {
#write.table(prod_data_all, paste(ALADYM_home, "/BEMTOOL_automatic_Production_data_", BMT_SPECIES[ALADYM_spe],"_SIM.csv", sep=""), sep=";", row.names=F)                  
#} else {
#write.table(prod_data_all, paste(ALADYM_home, "/BEMTOOL_automatic_Production_data_", BMT_SPECIES[ALADYM_spe],"_FORE.csv", sep=""), sep=";", row.names=F)                  
#}

 if (phase=="SIMULATION") {
       save_path <- paste(casestudy_path, "/Diagnosis/working files/", casestudy_name, " - Production data ",BMT_SPECIES[ALADYM_spe],".csv", sep="")
 } else {
 if (!MEY_CALCULATION) {
       save_path <- paste(casestudy_path, "/", harvest_rule_id,"/working files/", casestudy_name, " - Production data ", harvest_rule_id," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
} else {
       save_path <- paste(casestudy_path, "/MEY calculation/", harvest_rule_id,"/working files/", casestudy_name, " - Production data ", harvest_rule_id, " - ", BMT_SPECIES[ALADYM_spe],".csv", sep="")
}
 } 

write.table(prod_data_all, save_path, sep=";", row.names=F)
 
return(prod_data_all) 
}
