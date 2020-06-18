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
# Function to reload the values for the selectivity according to the 
# selection of the selectivity function
# ------------------------------------------------------------------------------
#
reload_species_table <- function(w) {

    species_casestudy  <<- NULL
   species_list <<- list()
speciesIndex <<- 0
add.species()
 species.model <<- gtkListStoreNew("gchararray", "gboolean")  
  
 for (i in 1:length(species_list)) {
    iter <-  species.model$append()$iter
   species.model$set(iter,0, species_list[[i]])
   # discards.model$set(iter, 1, discards_list[[i]]$month)           
       #  species.model$set(iter, 2, species_list[[i]][3])
     species.model$set(iter,1,TRUE)
  } 

 species.treeview <<- gtkTreeViewNewWithModel( species.model)
 species.treeview$setRulesHint(TRUE)
 species.treeview$getSelection()$setMode("single")
species.add_columns( species.treeview)
species.sw$destroy()
species.sw <<- gtkScrolledWindowNew(NULL, NULL)
species.sw$setShadowType("etched-in")
species.sw$setPolicy("automatic", "automatic")
species.sw$SetUsize(100, dim_big_tables)  
species.sw$add(species.treeview)
vbox_casestudy$packStart(species.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vbox_casestudy, species.sw, 3)
 
}
