# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


save_effort_files_fore<-function(loca_path) {

VESSELS_final_fore <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_FORECAST)*length(BMT_FLEETSEGMENTS)) +1)))
     VESSELS_final_fore[,1] <- c("Units: number", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep=""))     

      DAYS_final_fore <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_FORECAST)*length(BMT_FLEETSEGMENTS)) +1)))
     DAYS_final_fore[,1] <- c("Units: days", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep=""))     

       for (fl in 1:length(BMT_FLEETSEGMENTS)) {

      NUMBER_matrix_fore   <-  EFFORT_NUMBER_list_fore[[fl]] 
     DAY_matrix_fore   <-     EFFORT_DAY_list_fore[[fl]]
       
        VESSELS_final_fore[1,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] <- BMT_YEARS_FORECAST
         VESSELS_final_fore[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )]  <- ""
         VESSELS_final_fore[2,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat_fore <- as.data.frame(t(NUMBER_matrix_fore ) , stringsAsFactors =F) 
        VESSELS_final_fore[5:16,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] <-  mat_fore[2:13,]
           
              
   DAYS_final_fore[1,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] <- BMT_YEARS_FORECAST
         DAYS_final_fore[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )]  <- ""
         DAYS_final_fore[2,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat_fore <- as.data.frame(t(DAY_matrix_fore ) , stringsAsFactors =F) 
        DAYS_final_fore[5:16,  c( ( (fl-1)*length(BMT_YEARS_FORECAST)+2 ) : (fl*length(BMT_YEARS_FORECAST)+1 ) )] <-  mat_fore[2:13,]
 
  }
  
  
  path_of_vessel <- paste(getwd(), "/monthly data on number.csv", sep="")
       path_of_days <- paste(getwd(), "/monthly data on seadays.csv", sep="")

       if (!is.null(loca_path)) {
       loca_path_vessel <- paste(loca_path, "/monthly data on number.csv", sep="")
              loca_path_days <- paste(loca_path, "/monthly data on seadays.csv", sep="")
                  
      write.table(VESSELS_final_fore, file=loca_path_vessel, sep=";", row.names=F, col.names=F)
    write.table(DAYS_final_fore, file=loca_path_days, sep=";", row.names=F, col.names=F)
              }
                     
  write.table(VESSELS_final_fore, file=path_of_vessel, sep=";", row.names=F, col.names=F)
    write.table(DAYS_final_fore, file=path_of_days, sep=";", row.names=F, col.names=F)


    
      
#    temp_effcfg <-  as.data.frame(mat_cfg_EffortData , stringsAsFactors =F)
#    
#   levels(temp_effcfg[,2]) <- factor(c(levels(temp_effcfg[,2]), path_of_vessel))     
# temp_effcfg[2,2] <- path_of_vessel
# 
#    levels(temp_effcfg[,3]) <- factor(c(levels(temp_effcfg[,3]), path_of_days))     
# temp_effcfg[2,3] <- path_of_days
#   
#    mat_cfg_EffortData <<- temp_effcfg
}