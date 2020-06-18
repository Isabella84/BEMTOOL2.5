# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


save_effort_files<-function(directory_effort) {

VESSELS_final <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     VESSELS_final[,1] <- c("Units: number", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep=""))     

      DAYS_final <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     DAYS_final[,1] <- c("Units: days", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep=""))     

           GT_final <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     GT_final[,1] <- c("Units: GT", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep="")) 
     
           KW_final <- data.frame(matrix(nrow=16, ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     KW_final[,1] <- c("Units: KW", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", paste("casestudy.month", 1:12, sep="")) 

       for (fl in 1:length(BMT_FLEETSEGMENTS)) {

      NUMBER_matrix   <-  EFFORT_NUMBER_list[[fl]] 
     DAY_matrix   <-     EFFORT_DAY_list[[fl]]
     GT_matrix  <-    EFFORT_GT_list[[fl]]
       KW_matrix <-  EFFORT_KW_list[[fl]]
       
        VESSELS_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         VESSELS_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         VESSELS_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat <- as.data.frame(t(NUMBER_matrix ) , stringsAsFactors =F) 
        VESSELS_final[5:16,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:13,]
           
              
   DAYS_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         DAYS_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         DAYS_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat <- as.data.frame(t(DAY_matrix ) , stringsAsFactors =F) 
        DAYS_final[5:16,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:13,]
 
 
 GT_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         GT_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         GT_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat <- as.data.frame(t(GT_matrix ) , stringsAsFactors =F) 
        GT_final[5:16,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:13,]
 
 
 KW_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         KW_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         KW_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat <- as.data.frame(t(KW_matrix ) , stringsAsFactors =F) 
        KW_final[5:16,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:13,]

  }
      path_of_vessel <- paste(directory_effort, "\\monthly data on number.csv", sep="")
       path_of_days <- paste(directory_effort, "\\monthly data on seadays.csv", sep="")
       path_of_gt <- paste(directory_effort, "\\monthly data on GT.csv", sep="")
       path_of_kw <-  paste(directory_effort, "\\monthly data on KW.csv", sep="")
                     
  write.table(VESSELS_final, file=path_of_vessel, sep=";", row.names=F, col.names=F)
    write.table(DAYS_final, file=path_of_days, sep=";", row.names=F, col.names=F)
      write.table(GT_final, file=path_of_gt, sep=";", row.names=F, col.names=F)
        write.table(KW_final, file=path_of_kw, sep=";", row.names=F, col.names=F)   

    temp_effcfg <-  as.data.frame(mat_cfg_EffortData , stringsAsFactors =F)
    
   levels(temp_effcfg[,2]) <- factor(c(levels(temp_effcfg[,2]), path_of_vessel))     
 temp_effcfg[2,2] <- path_of_vessel
 
    levels(temp_effcfg[,3]) <- factor(c(levels(temp_effcfg[,3]), path_of_days))     
 temp_effcfg[2,3] <- path_of_days
  
     levels(temp_effcfg[,4]) <- factor(c(levels(temp_effcfg[,4]), path_of_gt))     
  temp_effcfg[2,4] <- path_of_gt
   
      levels(temp_effcfg[,5]) <- factor(c(levels(temp_effcfg[,5]), path_of_kw))     
   temp_effcfg[2,5] <- path_of_kw
   
    mat_cfg_EffortData <<- temp_effcfg
}