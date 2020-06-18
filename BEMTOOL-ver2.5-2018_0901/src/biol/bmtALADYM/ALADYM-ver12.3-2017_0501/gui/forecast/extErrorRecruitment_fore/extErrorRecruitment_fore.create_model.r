# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




extErrorRecruitment_fore.create_model <- function() {
# print("Creating model...")   
  # create list store
 # extErrorRecruitment_fore <<- list()
# extErrorRecruitment_foreIndex <<- 0
    add.extErrorRecruitment_fore()
  extErrorRecruitment_fore.model <<- gtkListStoreNew(rep("gdouble",(length(years_forecast)+1)), "gboolean")  

     for (i in 1:length(extErrorRecruitment_fore_list)) {
    iter <-  extErrorRecruitment_fore.model$append()$iter
    extErrorRecruitment_fore.model$set(iter,0, extErrorRecruitment_fore_list[[i]]$run_N)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(years_forecast)) {
        extErrorRecruitment_fore.model$set(iter, e, as.double(extErrorRecruitment_fore_list[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     extErrorRecruitment_fore.model$set(iter,(length(years_forecast)+1),TRUE)
  } 

}
