# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_landing_data_lists <- function() {

       for (fl in 1:length(BMT_FLEETSEGMENTS)) {
     
     spe_list <<- c()   
  for (sp in 1:length(BMT_SPECIES)) {

    landing_matrix <- try(data.frame(read.csv(as.character(mat_cfg_LandingData[2,(sp+1)]), sep=";", header=F))) 
     landing_matrix <-  landing_matrix[c(1, 4:16),]

     LANDING_matrix <- as.data.frame(t(landing_matrix[3:14,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    LANDING_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, LANDING_matrix)) 
      colnames(LANDING_matrix) <- c("year",MONTHS)    
  
    spe_list <<- c(spe_list, list(LANDING_matrix))
        
  }
  
  LANDING_list_all[[fl]] <<-  spe_list
 

  }
       
}