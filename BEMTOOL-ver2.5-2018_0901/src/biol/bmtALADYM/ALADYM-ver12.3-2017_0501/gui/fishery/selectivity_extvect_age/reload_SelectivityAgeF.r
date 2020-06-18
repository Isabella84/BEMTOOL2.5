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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_SelectivityAgeF <- function(w) {
                           
SelectivityAge_F_list <<- list()
SelectivityAge_FIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

         FF_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=(n_ages+1)))
      colnames(FF_matrix) <-   c("Year", paste("age", c(first_age_fem:(n_ages+first_age_fem-1)), sep="") )
         FF_matrix$Year <- years

   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityAge_F_list <<- c(SelectivityAge_F_list, list(FF_temp)) 
  }
  
  SelectivityAgeF_matrix <<- FF_matrix
   
  SelectivityAge_F.model <<- gtkListStoreNew( rep("gdouble", n_ages+1), "gboolean")  
  
   for (i in 1:length(SelectivityAge_F_list)) {
    iter <-  SelectivityAge_F.model$append()$iter
 SelectivityAge_F.model$set(iter,0, SelectivityAge_F_list[[i]]$Year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        SelectivityAge_F.model$set(iter, e, as.double(SelectivityAge_F_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityAge_F.model$set(iter,(n_ages+1),TRUE)
  } 

SelectivityAge_F.treeview$destroy()
SelectivityAge_F.treeview <<- gtkTreeViewNewWithModel( SelectivityAge_F.model)
SelectivityAge_F.treeview$setRulesHint(TRUE)
SelectivityAge_F.treeview$getSelection()$setMode("single")
SelectivityAge_F.add_columns( SelectivityAge_F.treeview)
SelectivityAge_F.sw$add(SelectivityAge_F.treeview)

}




reload_SelectivityAgeF <- function(w) {
                           
SelectivityAge_F_list <<- list()
SelectivityAge_FIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

FF_matrix <-  SelectivityAgeF_matrix 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityAge_F_list <<- c(SelectivityAge_F_list, list(FF_temp)) 
  }

  SelectivityAge_F.model <<- gtkListStoreNew( rep("gdouble", n_ages+1), "gboolean")  
  
   for (i in 1:length(SelectivityAge_F_list)) {
    iter <-  SelectivityAge_F.model$append()$iter
    SelectivityAge_F.model$set(iter,0, SelectivityAge_F_list[[i]]$Year)
   #print(paste("in model:", SelectivityAge_F_list[[i]]$Year))
    for (e in 1:n_ages) {
        SelectivityAge_F.model$set(iter, e, as.double(SelectivityAge_F_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
     # print(paste("in model:", as.double(SelectivityAge_F_list[[i]][e+1])))
    }
     SelectivityAge_F.model$set(iter,(n_ages+1),TRUE)
  } 

SelectivityAge_F.treeview$destroy()
SelectivityAge_F.treeview <<- gtkTreeViewNewWithModel( SelectivityAge_F.model)
SelectivityAge_F.treeview$setRulesHint(TRUE)
SelectivityAge_F.treeview$getSelection()$setMode("single")
SelectivityAge_F.add_columns( SelectivityAge_F.treeview)
SelectivityAge_F.sw$add(SelectivityAge_F.treeview)

}