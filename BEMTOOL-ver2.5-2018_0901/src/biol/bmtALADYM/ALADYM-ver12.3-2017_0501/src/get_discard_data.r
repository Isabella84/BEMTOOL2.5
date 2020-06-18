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

get_discard_data<-function() {

  nb_years <- length(years)
 
disc_data_all <- data.frame(matrix(nrow=0, ncol=4))
colnames(disc_data_all) <- c("Gear",	"Month",	"Discard",	"Unit")


for (n_disc in 1:length(FLEETSEGMENTS_names)) {
   disc_data_fleet <- data.frame(matrix(nrow=0, ncol=4))
colnames(disc_data_fleet) <- c("Gear",	"Month",	"Discard",	"Unit")

for (yy in 1:nb_years) {
    disc_data_temp <- as.numeric( as.character(FleetList_simulation[[n_disc]]@monthly.discard.vector[yy, 3:14] ))
    if (yy==1) {
    to_add <- data.frame(matrix(c(FLEETSEGMENTS_names[n_disc],0, FleetList_simulation[[n_disc]]@monthly.discard.vector[1,2], "kg"), nrow=1))
        colnames(to_add) <- c("Gear",	"Month",	"Discard",	"Unit")
    disc_data_fleet <- rbind(disc_data_fleet,  to_add)
    }
    to_add <- data.frame(cbind(rep(FLEETSEGMENTS_names[n_disc], 12), c((yy-1)*12 + c(1:12)), disc_data_temp, "kg") )
    colnames(to_add) <- c("Gear",	"Month",	"Discard",	"Unit")
    disc_data_fleet <- rbind(disc_data_fleet, to_add)
} # end years
 

disc_data_all <- rbind(disc_data_all, disc_data_fleet)

}  # end fleet loop 

write.table(disc_data_all, file=DISCARD_DATA_working_table, sep=";", row.names=F)

 
return(disc_data_all) 
}
