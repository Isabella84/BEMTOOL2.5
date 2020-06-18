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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_SelectivityAgeM_fore <- function(w) {
                           
SelectivityAge_M_fore_list <<- list()
SelectivityAge_M_foreIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

   FF_matrix <- data.frame(matrix(-1, nrow=length(years_forecast), ncol=(n_ages+1)))
      colnames(FF_matrix) <-   c("Year", paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
 FF_matrix$Year <- years_forecast
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityAge_M_fore_list <<- c(SelectivityAge_M_fore_list, list(FF_temp)) 
  }
  
  SelectivityAgeM_fore_matrix <<- FF_matrix
   
  SelectivityAge_M_fore.model <<- gtkListStoreNew(rep("gdouble", n_ages+1), "gboolean")   
  
   for (i in 1:length(SelectivityAge_M_fore_list)) {
    iter <-  SelectivityAge_M_fore.model$append()$iter
    SelectivityAge_M_fore.model$set(iter,0, SelectivityAge_M_fore_list[[i]]$Year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        SelectivityAge_M_fore.model$set(iter, e, as.double(SelectivityAge_M_fore_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityAge_M_fore.model$set(iter,(n_ages+1),TRUE)
  } 

SelectivityAge_M_fore.treeview$destroy()
SelectivityAge_M_fore.treeview <<- gtkTreeViewNewWithModel( SelectivityAge_M_fore.model)
SelectivityAge_M_fore.treeview$setRulesHint(TRUE)
SelectivityAge_M_fore.treeview$getSelection()$setMode("single")
SelectivityAge_M_fore.add_columns( SelectivityAge_M_fore.treeview)
SelectivityAge_M_fore.sw$add(SelectivityAge_M_fore.treeview)

}




reload_SelectivityAgeM_fore <- function(w) {
                           
SelectivityAge_M_fore_list <<- list()
SelectivityAge_M_foreIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

         FF_matrix <-  SelectivityAgeM_fore_matrix 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityAge_M_fore_list <<- c(SelectivityAge_M_fore_list, list(FF_temp)) 
  }

  SelectivityAge_M_fore.model <<- gtkListStoreNew(rep("gdouble", n_ages+1), "gboolean") 
  
   for (i in 1:length(SelectivityAge_M_fore_list)) {
    iter <-  SelectivityAge_M_fore.model$append()$iter
    SelectivityAge_M_fore.model$set(iter,0, SelectivityAge_M_fore_list[[i]]$Year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        SelectivityAge_M_fore.model$set(iter, e, as.double(SelectivityAge_M_fore_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityAge_M_fore.model$set(iter,(n_ages+1),TRUE)
  } 

SelectivityAge_M_fore.treeview$destroy()
SelectivityAge_M_fore.treeview <<- gtkTreeViewNewWithModel( SelectivityAge_M_fore.model)
SelectivityAge_M_fore.treeview$setRulesHint(TRUE)
SelectivityAge_M_fore.treeview$getSelection()$setMode("single")
SelectivityAge_M_fore.add_columns( SelectivityAge_M_fore.treeview)
SelectivityAge_M_fore.sw$add(SelectivityAge_M_fore.treeview)

}