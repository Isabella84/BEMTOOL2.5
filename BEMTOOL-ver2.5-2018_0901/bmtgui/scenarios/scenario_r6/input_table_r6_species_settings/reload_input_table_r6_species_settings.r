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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_input_table_r6_species_settings<- function(w) {

  input_table_r6_species_settings_list <<- list()
  input_table_r6_species_settingsIndex <<- 0

    if (is.null( input_table_r6_species_settings )) { 
   input_table_r6_species_settings_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=6))
   colnames(input_table_r6_species_settings_matrix) <- c("species", "% landing/catch", "SRR model", "F setting", "SSR", "Average years")
         input_table_r6_species_settings_matrix[,5] <- "N"
		input_table_r6_species_settings_matrix[,3] <- 1
	  input_table_r6_species_settings_matrix[,4] <- 1
     input_table_r6_species_settings_matrix[,1] <- BMT_SPECIES
 input_table_r6_species_settings <<- input_table_r6_species_settings_matrix
 } else {
     input_table_r6_species_settings_matrix <<- input_table_r6_species_settings 
 }
 
   for (r in 1:nrow(input_table_r6_species_settings_matrix)) { 
      input_table_r6_species_settings_temp <- as.list(input_table_r6_species_settings_matrix[r,]) 
      input_table_r6_species_settings_list <<- c(input_table_r6_species_settings_list, list(input_table_r6_species_settings_temp)) 
  }

input_table_r6_species_settings.model <<- gtkListStoreNew("gchararray", "gdouble", "gdouble", "gdouble" ,"gchararray", "gdouble", "gboolean")
  # add items 
  for (i in 1:length(input_table_r6_species_settings_list)) {
    iter <- input_table_r6_species_settings.model$append()$iter
    input_table_r6_species_settings.model$set(iter,0, input_table_r6_species_settings_list[[i]][1])
    input_table_r6_species_settings.model$set(iter, 1, as.double(input_table_r6_species_settings_list[[i]][2]) )
       input_table_r6_species_settings.model$set(iter, 2, as.numeric(input_table_r6_species_settings_list[[i]][3]) )
    input_table_r6_species_settings.model$set(iter, 3, as.numeric(input_table_r6_species_settings_list[[i]][4]) )
       input_table_r6_species_settings.model$set(iter, 4, input_table_r6_species_settings_list[[i]][5] )
    input_table_r6_species_settings.model$set(iter, 5, as.numeric(input_table_r6_species_settings_list[[i]][6]) )
     input_table_r6_species_settings.model$set(iter, 6,TRUE)
  } 

           
if (exists("input_table_r6_species_settings.treeview")) { 
input_table_r6_species_settings.treeview$destroy()
}
  
  input_table_r6_species_settings.treeview <<- gtkTreeViewNewWithModel( input_table_r6_species_settings.model)
 input_table_r6_species_settings.treeview$setRulesHint(TRUE)
 input_table_r6_species_settings.treeview$getSelection()$setMode("single")
input_table_r6_species_settings.add_columns( input_table_r6_species_settings.treeview)
input_table_r6_species_settings.sw$add(input_table_r6_species_settings.treeview)
    
}
