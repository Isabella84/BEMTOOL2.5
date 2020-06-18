# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_effort_data_lists <- function() {

    effort_vessels_matrix <- try(data.frame(read.csv(as.character(mat_cfg_EffortData[2,2]), sep=";", header=F))) 
    effort_days_matrix <- try(data.frame(read.csv(as.character(mat_cfg_EffortData[2,3]), sep=";", header=F))) 
     effort_gt_matrix <- try(data.frame(read.csv(as.character(mat_cfg_EffortData[2,4]), sep=";", header=F))) 
      effort_kw_matrix <- try(data.frame(read.csv(as.character(mat_cfg_EffortData[2,5]), sep=";", header=F)))
  
    effort_vessels_matrix <-  effort_vessels_matrix[c(1, 4:16),]
    effort_days_matrix <-  effort_days_matrix[c(1, 4:16),]
     effort_gt_matrix <-   effort_gt_matrix[c(1, 4:16),]
      effort_kw_matrix <-  effort_kw_matrix[c(1, 4:16),]
  
  for (fl in 1:length(BMT_FLEETSEGMENTS)) {

     NUMBER_matrix <- as.data.frame(t(effort_vessels_matrix[3:14,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    NUMBER_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, NUMBER_matrix)) 
      colnames(NUMBER_matrix) <- c("year",MONTHS)    
    
        DAY_matrix <-  as.data.frame(t(effort_days_matrix[3:14,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] )  , stringsAsFactors =F)          
    DAY_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, DAY_matrix)) 
      colnames(DAY_matrix) <- c("year",MONTHS)
      
           GT_matrix <-  as.data.frame(t(effort_gt_matrix[3:14,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] )  , stringsAsFactors =F)          
    GT_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, GT_matrix)) 
      colnames(GT_matrix) <- c("year",MONTHS)
      
           KW_matrix <-  as.data.frame(t(effort_kw_matrix[3:14,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] )  , stringsAsFactors =F)          
    KW_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, KW_matrix)) 
      colnames(KW_matrix) <- c("year",MONTHS)  
  
  EFFORT_NUMBER_list[[fl]] <<-  NUMBER_matrix
    EFFORT_DAY_list[[fl]] <<-  DAY_matrix
      EFFORT_GT_list[[fl]] <<-  GT_matrix
        EFFORT_KW_list[[fl]] <<-  KW_matrix
        
  }

       
}