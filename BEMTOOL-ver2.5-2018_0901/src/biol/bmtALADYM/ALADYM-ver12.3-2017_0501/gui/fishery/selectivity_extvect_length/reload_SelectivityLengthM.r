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
reload_EMPTY_SelectivityLengthM <- function(w) {
                           
SelectivityLength_M_list <<- list()
SelectivityLength_MIndex <<- 0


if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[3,3])      
} 

l_inf_lens <- c(0:(round(l_inf,0)+1))

         FF_matrix <- data.frame(matrix(-1, nrow=length(l_inf_lens) , ncol=(length(years)+1)))
colnames(FF_matrix) <-   c("Length", years )
         FF_matrix$Length <- l_inf_lens

   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityLength_M_list <<- c(SelectivityLength_M_list, list(FF_temp)) 
  }
  
  SelectivityLengthM_matrix <<- FF_matrix
   
  SelectivityLength_M.model <<- gtkListStoreNew(rep("gdouble",(length(years)+1)), "gboolean")  
  
   for (i in 1:length(SelectivityLength_M_list)) {
    iter <-  SelectivityLength_M.model$append()$iter
    SelectivityLength_M.model$set(iter,0, SelectivityLength_M_list[[i]]$Length)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years)) {
        SelectivityLength_M.model$set(iter, e, as.double(SelectivityLength_M_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityLength_M.model$set(iter,(length(years)+1),TRUE)
  } 

SelectivityLength_M.treeview$destroy()
SelectivityLength_M.treeview <<- gtkTreeViewNewWithModel( SelectivityLength_M.model)
SelectivityLength_M.treeview$setRulesHint(TRUE)
SelectivityLength_M.treeview$getSelection()$setMode("single")
SelectivityLength_M.add_columns( SelectivityLength_M.treeview)
SelectivityLength_M.sw$add(SelectivityLength_M.treeview)

}




reload_SelectivityLengthM <- function(w) {
                           
SelectivityLength_M_list <<- list()
SelectivityLength_MIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[3,3])      
} 

l_inf_lens <- c(0:(round(l_inf,0)+1))


         FF_matrix <-  SelectivityLengthM_matrix 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityLength_M_list <<- c(SelectivityLength_M_list, list(FF_temp)) 
  }

  SelectivityLength_M.model <<- gtkListStoreNew(rep("gdouble", (length(years)+1)), "gboolean")  
  
   for (i in 1:length(SelectivityLength_M_list)) {
    iter <-  SelectivityLength_M.model$append()$iter
     SelectivityLength_M.model$set(iter,0, SelectivityLength_M_list[[i]]$Length)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years)) {
        SelectivityLength_M.model$set(iter, e, as.double(SelectivityLength_M_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityLength_M.model$set(iter,(length(years)+1),TRUE)
  } 

SelectivityLength_M.treeview$destroy()
SelectivityLength_M.treeview <<- gtkTreeViewNewWithModel( SelectivityLength_M.model)
SelectivityLength_M.treeview$setRulesHint(TRUE)
SelectivityLength_M.treeview$getSelection()$setMode("single")
SelectivityLength_M.add_columns( SelectivityLength_M.treeview)
SelectivityLength_M.sw$add(SelectivityLength_M.treeview)

}