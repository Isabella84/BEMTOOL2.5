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
reload_EMPTY_SelectivityLengthF <- function(w) {
                           
SelectivityLength_F_list <<- list()
SelectivityLength_FIndex <<- 0

 
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[6,3])      
} 

l_inf_lens <-c(0:(round(l_inf,0)+1))


         FF_matrix <- data.frame(matrix(-1, nrow=length(l_inf_lens) , ncol=(length(years)+1)))
      colnames(FF_matrix) <-  c("Length", years )
         FF_matrix$Length <- l_inf_lens

   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityLength_F_list <<- c(SelectivityLength_F_list, list(FF_temp)) 
  }
  
  SelectivityLengthF_matrix <<- FF_matrix
   
  SelectivityLength_F.model <<- gtkListStoreNew( rep("gdouble", (length(years)+1)), "gboolean")  
  
   for (i in 1:length(SelectivityLength_F_list)) {
    iter <-  SelectivityLength_F.model$append()$iter
 SelectivityLength_F.model$set(iter,0, SelectivityLength_F_list[[i]]$Length)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years)) {
        SelectivityLength_F.model$set(iter, e, as.double(SelectivityLength_F_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     SelectivityLength_F.model$set(iter,(length(years)+1),TRUE)
  } 

SelectivityLength_F.treeview$destroy()
SelectivityLength_F.treeview <<- gtkTreeViewNewWithModel( SelectivityLength_F.model)
SelectivityLength_F.treeview$setRulesHint(TRUE)
SelectivityLength_F.treeview$getSelection()$setMode("single")
SelectivityLength_F.add_columns( SelectivityLength_F.treeview)
SelectivityLength_F.sw$add(SelectivityLength_F.treeview)

}




reload_SelectivityLengthF <- function(w) {
                           
SelectivityLength_F_list <<- list()
SelectivityLength_FIndex <<- 0

 
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[6,3])      
} 

l_inf_lens <-c(0:(round(l_inf,0)+1))


FF_matrix <-  SelectivityLengthF_matrix 
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityLength_F_list <<- c(SelectivityLength_F_list, list(FF_temp)) 
  }

  SelectivityLength_F.model <<- gtkListStoreNew( rep("gdouble", length(years)+1), "gboolean")  
  
   for (i in 1:length(SelectivityLength_F_list)) {
    iter <-  SelectivityLength_F.model$append()$iter
    SelectivityLength_F.model$set(iter,0, SelectivityLength_F_list[[i]]$Length)
   #print(paste("in model:", SelectivityAge_F_list[[i]]$Year))
    for (e in 1:length(years)) {
        SelectivityLength_F.model$set(iter, e, as.double(SelectivityLength_F_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
     # print(paste("in model:", as.double(SelectivityAge_F_list[[i]][e+1])))
    }
     SelectivityLength_F.model$set(iter,(length(years)+1),TRUE)
  } 

SelectivityLength_F.treeview$destroy()
SelectivityLength_F.treeview <<- gtkTreeViewNewWithModel( SelectivityLength_F.model)
SelectivityLength_F.treeview$setRulesHint(TRUE)
SelectivityLength_F.treeview$getSelection()$setMode("single")
SelectivityLength_F.add_columns( SelectivityLength_F.treeview)
SelectivityLength_F.sw$add(SelectivityLength_F.treeview)

}