# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



correction_stock_nb_from_VIT <- function(m_spe,num_classes_BACKUP) {
nb_classes = min(num_classes_BACKUP) #length(VITinfo[[m_spe]][[y_int]]$results[[1]]$age_classes)
prop = data.frame (matrix(NA,nrow = nb_classes, ncol = simperiod))
  for (y_int in 1:simperiod)    {
  
  prop[,y_int]= data.frame(as.numeric(VITinfo[[m_spe]][[y_int]]$results[[1]]$VPA_results_nb[c(1:nb_classes),3] )) / data.frame(as.numeric(VITinfo[[m_spe]][[y_int]]$results[[1]]$VPA_results_nb[c(1:nb_classes),2]) )
  }
prop$average = rowSums(prop) /ncol(prop)
return(prop)

}