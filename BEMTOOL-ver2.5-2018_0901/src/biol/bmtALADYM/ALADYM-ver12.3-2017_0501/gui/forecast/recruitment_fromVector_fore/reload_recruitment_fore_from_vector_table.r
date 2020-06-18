# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
# Function to reload the values for the recruitment_fore_from_vector according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_recruitment_fore_from_vector_table <- function(w) {
  #recruitment_fore_from_vector.vector   <<- NULL
  recruitments_fore_from_vector <<- list()
recruitments_fore_from_vector_index <<- 0

   sr_matrix <- data.frame(matrix( nrow=length(years_forecast), ncol=12))
   colnames(sr_matrix) <- c("year",MONTHS)
     sr_matrix$year <- years
   for (r in 1:nrow(sr_matrix)) { 
  sr_temp <- as.list(sr_matrix[r,]) 
  recruitments_fore_from_vector <<- c(recruitments_fore_from_vector, list(sr_temp)) 
  }
  
  recruitments_fore_from_vector.vector <<- sr_matrix

 recruitments_fore_from_vector.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments_fore_from_vector)) {
    iter <-  recruitments_fore_from_vector.model$append()$iter
     recruitments_fore_from_vector.model$set(iter,0, recruitments_fore_from_vector[[i]]$year)
    #print(paste("in model:", as.character(recruitments_fore_from_vector[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments_fore_from_vector.model$set(iter, e, as.double(recruitments_fore_from_vector[[i]][e+1]))          
       #print(paste("in model:", recruitments_fore_from_vector[[i]][e]) )
    }
     recruitments_fore_from_vector.model$set(iter,13,TRUE)
  } 

 recruitments_fore_from_vector.treeview <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model)
 recruitments_fore_from_vector.treeview$setRulesHint(TRUE)
 recruitments_fore_from_vector.treeview$getSelection()$setMode("single")
recruitments_fore_from_vector.add_columns( recruitments_fore_from_vector.treeview)
recruitments_fore_from_vector.sw$destroy()
recruitments_fore_from_vector.sw <<- gtkScrolledWindowNew(NULL, NULL)
recruitments_fore_from_vector.sw$setShadowType("etched-in")
recruitments_fore_from_vector.sw$setPolicy("automatic", "automatic")
recruitments_fore_from_vector.sw$SetUsize(100, dim_eff_tables)  
recruitments_fore_from_vector.sw$add(recruitments_fore_from_vector.treeview)
vboxSRrelationship$packStart(recruitments_fore_from_vector.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vboxSRrelationship, recruitments_fore_from_vector.sw, 4)
gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)
    
}
