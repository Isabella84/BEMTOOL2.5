# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




get_fishing_coefficient<-function() {

  nb_years <- length(years)

# Table of effort data (Gear	Month	Vessels	Days	GT )   es: Trawler1	0	20	300	10
fc_all <- data.frame(matrix(nrow=(length(years)*12)+1, ncol=length(FLEETSEGMENTS_names)))

for (n_eff in 1:length(FLEETSEGMENTS_names)) {

for (yy in 1:nb_years) {

if (yy==1) {
        fc_all[1,n_eff] <- as.numeric( as.character(FleetList_simulation[[n_eff]]@fishingeffort.vector[1,2] ))
}
        fc_all[(yy-1)*12 + c(2:13),n_eff] <- as.numeric( as.character(FleetList_simulation[[n_eff]]@fishingeffort.vector[yy, 3:14] ))
} # end year

}

write.table(fc_all, file=FISHING_COEFF_working_table, sep=";", row.names=F)



return(fc_all) 
}
