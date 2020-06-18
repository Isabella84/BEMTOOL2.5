# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_effort_data_lists_fore <- function() {

# only vesselssssssssssssss
    if (BMT_SCENARIO != BMT_HR_STATUS_QUO) {
    effort_vessels_matrix_fore <- try(read.csv(as.character(mat_cfg_scenario_settings_fore_options[[1]]), sep=";", header=F)) 
      } else {
    effort_vessels_matrix_fore <- NULL  
      }
      
  if ( class(effort_vessels_matrix_fore) != "try-error" & !is.null(effort_vessels_matrix_fore) ) {
        effort_vessels_matrix_fore <- data.frame(effort_vessels_matrix_fore) 
    effort_vessels_matrix_fore <-  effort_vessels_matrix_fore[c(1, 4:16),]
  
  for (fl in 1:length(BMT_FLEETSEGMENTS)) {

     NUMBER_matrix <- as.data.frame(t(effort_vessels_matrix_fore[3:14,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] ) , stringsAsFactors =F)           
    NUMBER_matrix <- data.frame(cbind(BMT_YEARS_FORECAST, NUMBER_matrix)) 
      colnames(NUMBER_matrix) <- c("year",MONTHS)    
  EFFORT_NUMBER_list_fore[[fl]] <<-  NUMBER_matrix
        
  }
  
 } else {
 
   for (fl in 1:length(BMT_FLEETSEGMENTS)) {
            last_years_vess <- EFFORT_NUMBER_list[[fl]]
     NUMBER_matrix <- as.data.frame(cbind(BMT_YEARS_FORECAST, last_years_vess[nrow(last_years_vess),2:13]))                     
    NUMBER_matrix[,1] <- BMT_YEARS_FORECAST 
      colnames(NUMBER_matrix) <- c("year",MONTHS)    
  EFFORT_NUMBER_list_fore[[fl]] <<-  NUMBER_matrix     
  }

 }
 
 # only dayssssssssssss
 
     if (BMT_SCENARIO != BMT_HR_STATUS_QUO) {
    effort_days_matrix_fore <- try(read.csv(as.character(mat_cfg_scenario_settings_fore_options[[2]]), sep=";", header=F)) 
     } else {
     effort_days_matrix_fore <- NULL
     }
  
  if ( class(effort_days_matrix_fore) != "try-error" & !is.null(effort_days_matrix_fore) ) {
    effort_days_matrix_fore <- data.frame(effort_days_matrix_fore)
    effort_days_matrix_fore <-  effort_days_matrix_fore[c(1, 4:16),]
  
  for (fl in 1:length(BMT_FLEETSEGMENTS)) {
        DAY_matrix <-  as.data.frame(t(effort_days_matrix_fore[3:14,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] )  , stringsAsFactors =F)          
    DAY_matrix <- data.frame(cbind(BMT_YEARS_FORECAST, DAY_matrix)) 
      colnames(DAY_matrix) <- c("year",MONTHS)
    EFFORT_DAY_list_fore[[fl]] <<-  DAY_matrix      
  }
  
 } else {
 
  for (fl in 1:length(BMT_FLEETSEGMENTS)) {
         last_years_days <- EFFORT_DAY_list[[fl]]
        DAY_matrix <-   as.data.frame(cbind(BMT_YEARS_FORECAST, last_years_days[nrow(last_years_days),2:13]))     
        DAY_matrix[,1] <- BMT_YEARS_FORECAST
      colnames(DAY_matrix) <- c("year",MONTHS)  
    EFFORT_DAY_list_fore[[fl]] <<-  DAY_matrix      
  }

 }
 

       
}