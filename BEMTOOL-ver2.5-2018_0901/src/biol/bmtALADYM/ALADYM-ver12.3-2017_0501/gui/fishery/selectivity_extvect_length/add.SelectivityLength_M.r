# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#
# ------------------------------------------------------------------------------
# add elements to the list of total mortality values (MALES)
# ------------------------------------------------------------------------------
#
add.SelectivityLength_M <- function() {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[3,3])      
} 

l_inf_lens <-c(0:(round(l_inf,0)+1))

#print("Adding elements to the list...")   
  if (!is.null(SelectivityLengthM_matrix)) {
  for (r in 1:nrow(SelectivityLengthM_matrix)) {
  FM_temp <- as.list(SelectivityLengthM_matrix[r,]) 
  names(FM_temp) <- c("Length", years )
 # names(FM_temp) <-  c(paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep=""))
  SelectivityLength_M_list <<- c(SelectivityLength_M_list, list(FM_temp)) 
  }
   } else {
   FM_matrix <- data.frame(matrix(-1, nrow=length(l_inf_lens) , ncol=(length(years)+1)))
   colnames(FM_matrix) <- c("Length", years )
     FM_matrix$Length <- l_inf_lens
#  colnames(FM_matrix) <-  c(paste("age", c(first_age_mal:(n_ages+first_age_mal-1)), sep="") )
     
   for (r in 1:nrow(FM_matrix)) { 
  FM_temp <- as.list(FM_matrix[r,]) 
  SelectivityLength_M_list <<- c(SelectivityLength_M_list, list(FM_temp)) 
  }
 }
# print("Fishing mortality (MALES) successfully added to the list!", quote=F)
}

