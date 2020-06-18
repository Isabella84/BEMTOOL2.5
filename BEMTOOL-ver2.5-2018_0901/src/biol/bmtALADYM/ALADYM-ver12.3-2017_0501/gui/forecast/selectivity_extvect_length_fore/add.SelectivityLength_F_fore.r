# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
# ------------------------------------------------------------------------------
# add elements to the list of total mortality values (FEMALES)
# ------------------------------------------------------------------------------
#
add.SelectivityLength_F_fore <- function() {


if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[6,3])      
} 

l_inf_lens <-c(0:(round(l_inf,0)+1))

#print("Adding elements to the list...")   
  if (!is.null(SelectivityLengthF_matrix)) {
  for (r in 1:nrow(SelectivityLengthF_matrix)) {
  FF_temp <- as.list(SelectivityLengthF_matrix[r,]) 
 names(FF_temp) <- c("Length", years_forecast )
   #   names(FF_temp) <-  c(paste("age", c(first_age_fem:(n_ages+first_age_fem-1)), sep="") )
  SelectivityLength_F_fore_list <<- c(SelectivityLength_F_fore_list, list(FF_temp)) 
  }
   } else {
   FF_matrix <- data.frame(matrix(-1, nrow=length(l_inf_lens) , ncol=(length(years_forecast)+1)))
   colnames(FF_matrix) <- c("Length", years_forecast )
     FF_matrix$Length <- l_inf_lens
       #  colnames(FF_matrix) <-   c(paste("age", c(first_age_fem:(n_ages+first_age_fem-1)), sep="") )
    
   for (r in 1:nrow(FF_matrix)) { 
  FF_temp <- as.list(FF_matrix[r,]) 
  SelectivityLength_F_fore_list <<- c(SelectivityLength_F_fore_list, list(FF_temp)) 
  }
 }
 #print("Fishing mortality (FEMALES) successfully added to the list!", quote=F)
}

