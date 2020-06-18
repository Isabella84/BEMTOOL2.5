# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




get_effort_data<-function() {

  nb_years <- length(years)
  
  
  
# Table of effort data (Gear	Month	Vessels	Days	GT )   es: Trawler1	0	20	300	10
eff_data_all <- data.frame(matrix(nrow=0, ncol=5))
colnames(eff_data_all) <- c("Gear",	"Month",	"Vessels",	"Days", "GT")

if (IN_BEMTOOL) {
FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
}

for (n_eff in 1:length(FLEETSEGMENTS_names)) {

eff_data_fleet <- data.frame(matrix(nrow=0, ncol=5))
colnames(eff_data_fleet) <- c("Gear",	"Month",	"Vessels",	"Days", "GT")



for (yy in 1:nb_years) {

    vessel_temp <- as.numeric( as.character(FleetList_simulation[[n_eff]]@vessels.vector[yy, 3:14] ))
    day_temp <-  as.numeric( as.character(FleetList_simulation[[n_eff]]@days.vector[yy, 3:14]))
    gt_temp <-  as.numeric( as.character(FleetList_simulation[[n_eff]]@gt.vector[yy, 3:14] ))
   # }

    if (yy==1) {
    to_add <- data.frame(matrix(c(FLEETSEGMENTS_names[n_eff],0,  as.numeric( as.character(FleetList_simulation[[n_eff]]@vessels.vector[1,2] )),  as.numeric( as.character(FleetList_simulation[[n_eff]]@days.vector[1, 2])), as.numeric( as.character(FleetList_simulation[[n_eff]]@gt.vector[1,2] ))), nrow=1))
    colnames(to_add) <-c("Gear",	"Month",	"Vessels",	"Days", "GT")
    eff_data_fleet <- rbind(eff_data_fleet,  to_add)
    }
    to_add <- data.frame(cbind(rep(FLEETSEGMENTS_names[n_eff], 12), c((yy-1)*12 + c(1:12)), vessel_temp, day_temp, gt_temp) )
    colnames(to_add) <-c("Gear",	"Month",	"Vessels",	"Days", "GT")
    eff_data_fleet <- rbind(eff_data_fleet, to_add)
} # end years


eff_data_all <- rbind(eff_data_all, eff_data_fleet)
}  # end fleet loop

eff_data_all_temp <- eff_data_all

colnames(eff_data_all_temp) <- c("fleet_segment",	"Month",	"VESSELS",	"DAYS", "GT")
write.table(eff_data_all_temp, EFFORT_DATA_working_table, sep=";", row.names=F)

return(eff_data_all) 
}
