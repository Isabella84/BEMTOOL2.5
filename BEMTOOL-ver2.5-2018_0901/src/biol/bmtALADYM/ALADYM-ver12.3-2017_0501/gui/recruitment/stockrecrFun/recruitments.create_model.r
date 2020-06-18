# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
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
# create model for the tree of stock-recruitment
# ------------------------------------------------------------------------------
#
recruitments.create_model <- function() {
#print("Creating model...")   
  # create list store
  recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  add.recruitments()
  # add items 
  for (i in 1:length(recruitments)) {
    iter <- recruitments.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    recruitments.model$set(iter,0, recruitments[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         recruitments.model$set(iter, e, as.numeric(recruitments[[i]][e+1]))
    }
       recruitments.model$set(iter, 13,TRUE)
  } 
  #print("Recruitment Model successfully created!")  
}

