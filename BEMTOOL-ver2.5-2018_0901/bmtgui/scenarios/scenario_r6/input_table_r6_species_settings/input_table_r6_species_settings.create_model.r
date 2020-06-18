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
# ------------------------------------------------------------------------------
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
input_table_r6_species_settings.create_model <- function() {
#print("Creating model...")   
  # create list storerep("gdouble", 5)
  input_table_r6_species_settings.model <<- gtkListStoreNew("gchararray", "gdouble", "gdouble", "gdouble" ,"gchararray", "gdouble", "gboolean")  
  add.input_table_r6_species_settings()
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
  #print("input_table_r6_species_settings Model successfully created!")  
}
