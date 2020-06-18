# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





extErrorRecruitment.create_model <- function() {
# print("Creating model...")   
  # create list store
 # extErrorRecruitment <<- list()
# extErrorRecruitmentIndex <<- 0
    add.extErrorRecruitment()
  extErrorRecruitment.model <<- gtkListStoreNew(rep("gdouble",CI_NB_RUNS), "gboolean")  

  # add items 
  iter <- extErrorRecruitment.model$append()$iter
  for (i in c(1:nrow(CI_external_matrix))) {
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    extErrorRecruitment.model$set(iter,(i-1), as.double(extErrorRecruitment_list[[i]]))
    #print(i)
      }   
   extErrorRecruitment.model$set(iter, i,TRUE)

}
