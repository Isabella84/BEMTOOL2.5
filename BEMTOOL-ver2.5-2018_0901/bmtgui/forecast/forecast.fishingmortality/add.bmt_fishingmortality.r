# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#
#
#
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.bmt_fishingmortality <- function() {
#print("Adding elements to the list...")   
 index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_diagnosis_species_pressure)
  index_to_update <- which(BMT_SPECIES == selected)    
names_of_fleets <- mat_cfg_SF[index_to_update,2:ncol(mat_cfg_SF)]

  if (!is.null(bmt_fishingmortality)) {
    for (r in 1:nrow(bmt_fishingmortality)) {
        reduction_temp <- as.list(bmt_fishingmortality[r,]) 
        names(reduction_temp) <- c("Year", paste("% F to", names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ]))
        bmt_fishingmortality_list <<- c(bmt_fishingmortality_list, list(reduction_temp)) 
    }
   } #else {
# reduction_matrix <- data.frame(matrix("", nrow=length(BMT_YEARS_SIMULATION), ncol=0 ) )
#  # colnames(reduction_matrix) <- c("Year", paste("% F to", names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ]))
#   #   reduction_matrix$Year <- BMT_YEARS_SIMULATION
#  bmt_fishingmortality <<- reduction_matrix   
# }
#print("bmt_fishingmortality (simulation) list successfully updated!", quote=F)
}