# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

if (phase == "SIMULATION") {
 years_to_loop <- years
} else {
 years_to_loop <- c(years, years.forecast)
} 

 ALL_ALADYM <- TRUE
      for (m_spe in 1:length(BMT_SPECIES)) {
          if (ALL_ALADYM) {
          ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".AladymSimulation", sep=""),1])
          ALL_ALADYM <- ALADYM_flag
          } 
      }
      
   changes_mort <- data.frame(matrix(nrow=length(years_to_loop), ncol=(length(BMT_FLEETSEGMENTS)+1)))  
   colnames(changes_mort) <- c("Year", paste("Annual_reduction_", BMT_FLEETSEGMENTS, sep="")) 
   changes_mort$Year <- years_to_loop
      if (ALL_ALADYM) {
       for (m_spe in 1:length(BMT_SPECIES)) {
            ALADYM_spe <<- m_spe
            
            source( paste(ALADYM_home, "/src/paths.r", sep="") )
            mortalities=read.table(MORTALITYCHANGE_table,header=T,sep=";")
                                
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
            
            changes_mort_spe<- data.frame(matrix(nrow=length(years_to_loop), ncol=(length(BMT_FLEETSEGMENTS)+1)))  
            colnames(changes_mort_spe) <- c("Year", paste("Annual_reduction_", BMT_FLEETSEGMENTS, sep="")) 
            changes_mort_spe$Year <- years_to_loop
            
            fl_se_ord <- 1
            for (fl_se in 1:length(BMT_FLEETSEGMENTS)) {
                 if (fl_se %in% associated_fleetsegment_indices) {
                      changes_mort_spe[,fl_se+1]  <- mortalities[,fl_se_ord+1]
                      fl_se_ord <- fl_se_ord + 1           
                 }    
            }
            
            if (m_spe == 1) {
                  mortalities_changes_list <- list(changes_mort_spe) 
            } else {
                  mortalities_changes_list <- c(mortalities_changes_list, list(changes_mort_spe)) 
            }     
      }

      }

        for (fl_se in 1: length(BMT_FLEETSEGMENTS)) { 
         values_reduction <- data.frame(matrix(nrow=length(years_to_loop), ncol=0))
         for (m_spe in 1:length(BMT_SPECIES)){
          values_reduction <- cbind(values_reduction, mortalities_changes_list[[m_spe]][,fl_se+1])
         }  
          changes_mort[, fl_se+1] <- rowMeans(values_reduction, na.rm = T)
        }

         time_span <- as.numeric(as.character(cfg[rownames(cfg)==paste("casestudy.HR", BMT_SCENARIO, sep=""), 1]))   

         if ((simperiod+time_span+1) < (simperiod+foreperiod) ) {
         changes_mort[(simperiod+time_span+1):(simperiod+foreperiod),2:ncol(changes_mort)] <- 0
         }
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
# if (phase=="SIMULATION") {
#       save_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\[", casestudy_name, "] Average mortalities reductions SIM.csv", sep="")
# } else {
# if (!MEY_CALCULATION) {
#       save_path <- paste(casestudy_path, "\\", harvest_rule_id,"\\ALADYM\\[", casestudy_name, "] Average mortalities reductions FORE ", harvest_rule_id,".csv", sep="")
# } else {
#       save_path <- paste(casestudy_path, "\\MEY calculation\\", harvest_rule_id,"\\ALADYM\\[", casestudy_name, "] Average mortalities reduction FORE ", harvest_rule_id,".csv", sep="")
# }
# }   
write.table(changes_mort, MORTALITYCHANGEALLSPECIES_table,row.names=FALSE, sep=";")
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
