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
add.input_table_r6_species_settings <- function() {
#print("Adding elements to the list...")   
  if (!is.null(input_table_r6_species_settings)) {
  for (r in 1:nrow(input_table_r6_species_settings)) {
  input_table_r6_species_settings_temp <- as.list(input_table_r6_species_settings[r,]) 
  names(input_table_r6_species_settings_temp) <- c("species", "% landing/catch", "SRR model", "F setting", "SSR", "Average years")
  input_table_r6_species_settings_list <<- c(input_table_r6_species_settings_list, list(input_table_r6_species_settings_temp)) 
  }
   } else {
   input_table_r6_species_settings_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=6))
   colnames(input_table_r6_species_settings_matrix) <- c("species", "% landing/catch", "SRR model", "F setting", "SSR", "Average years")
      input_table_r6_species_settings_matrix[,5] <- "N"
	  input_table_r6_species_settings_matrix[,3] <- 1
	  input_table_r6_species_settings_matrix[,4] <- 1
     input_table_r6_species_settings_matrix[,1] <- BMT_SPECIES
   for (r in 1:nrow(input_table_r6_species_settings_matrix)) { 
  input_table_r6_species_settings_temp <- as.list(input_table_r6_species_settings_matrix[r,]) 
  input_table_r6_species_settings_list <<- c(input_table_r6_species_settings_list, list(input_table_r6_species_settings_temp)) 
  }
 }
#print("input_table_r6_species_settings (simulation) list successfully updated!", quote=F)
}