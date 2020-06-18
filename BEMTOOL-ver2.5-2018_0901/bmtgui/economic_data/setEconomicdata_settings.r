# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


setEconomicdata_settings <- function() {

# temp <- mat_cfg_species_settings

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_economicdata_fleet_combo)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update <- which(BMT_FLEETSEGMENTS == selected)    

if (!is.null(selected) ) {
ECONOMICDATA_COSTS_list[[index_to_update]] <<- bmt_economic.COSTS 
ECONOMICDATA_OTHERS_list[[index_to_update]] <<-  bmt_economic.OTHERS  
ECONOMICDATA_REVENUES_list[[index_to_update]] <<-  bmt_economic.REVENUES
ECONOMICDATA_REVENUES_discard_list[[index_to_update]] <<-  bmt_economic.REVENUES_discard


bmt_save_economicdata_file()

}
}



set_economic_data_lists <- function() {

    economicdata_matrix <- try(data.frame(read.csv(as.character(mat_cfg_EffortData[4,2]), sep=";", header=F, stringsAsFactors =F))) 
    economicdata_matrix <-  economicdata_matrix[c(1, 4:nrow(economicdata_matrix)),]
      
       rest_ <- economicdata_matrix[!(as.character(economicdata_matrix[,1]) %in% COSTS_vector),]
    economicdata_matrix_costs <-  economicdata_matrix[as.character(economicdata_matrix[,1]) %in% COSTS_vector,]
       rest_ <- economicdata_matrix[!(as.character(economicdata_matrix[,1]) %in% OTHERS_vector) & !(as.character(economicdata_matrix[,1]) %in% COSTS_vector),]
     economicdata_matrix_others <-  economicdata_matrix[as.character(economicdata_matrix[,1]) %in% OTHERS_vector,]

   economicdata_matrix_revenues_byspecies <- rest_[3:(length(BMT_SPECIES)+2),] 
          rest_ <- economicdata_matrix[!(as.character(economicdata_matrix[,1]) %in% economicdata_matrix_revenues_byspecies[,1]) & !(as.character(economicdata_matrix[,1]) %in% OTHERS_vector) & !(as.character(economicdata_matrix[,1]) %in% COSTS_vector),]
          
           economicdata_matrix_revenues_byspecies_discard <- rest_[3:nrow(rest_),]
            
  for (fl in 1:length(BMT_FLEETSEGMENTS)) {

  economicdata_matrix_costs[ economicdata_matrix_costs[] == ""] <- NA
     COSTS_matrix <- as.data.frame(t(economicdata_matrix_costs[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    COSTS_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, COSTS_matrix)) 
      colnames(COSTS_matrix) <- c("year",t(as.character(economicdata_matrix_costs[,  1]) )  )    
  
    economicdata_matrix_others[ economicdata_matrix_others[] == ""] <- NA  
      OTHERS_matrix <- as.data.frame(t(economicdata_matrix_others[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    OTHERS_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, OTHERS_matrix)) 
      colnames(OTHERS_matrix) <- c("year",t(as.character(economicdata_matrix_others[,  1]) )  )    
  
      economicdata_matrix_revenues_byspecies[ economicdata_matrix_revenues_byspecies[] == ""] <- NA      
     REVENUES_BYSPECIES_matrix <-  as.data.frame(t(economicdata_matrix_revenues_byspecies[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    REVENUES_BYSPECIES_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, REVENUES_BYSPECIES_matrix)) 
      colnames(REVENUES_BYSPECIES_matrix) <- c("year",t(as.character(economicdata_matrix_revenues_byspecies[,  1]) )  )
  
    economicdata_matrix_revenues_byspecies_discard[ economicdata_matrix_revenues_byspecies_discard[] == ""] <- NA      
     REVENUES_BYSPECIES_disc_matrix <-  as.data.frame(t(economicdata_matrix_revenues_byspecies_discard[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] ) , stringsAsFactors =F)           
    REVENUES_BYSPECIES_disc_matrix <- data.frame(cbind(BMT_YEARS_SIMULATION, REVENUES_BYSPECIES_disc_matrix)) 
      colnames(REVENUES_BYSPECIES_disc_matrix) <- c("year",t(as.character(economicdata_matrix_revenues_byspecies_discard[,1]) )  )

  
  
  ECONOMICDATA_COSTS_list[[fl]] <<-  COSTS_matrix
    ECONOMICDATA_REVENUES_list[[fl]] <<-  REVENUES_BYSPECIES_matrix
     ECONOMICDATA_REVENUES_discard_list[[fl]] <<-  REVENUES_BYSPECIES_disc_matrix
      ECONOMICDATA_OTHERS_list[[fl]] <<-  OTHERS_matrix      
  }
       
}



bmt_save_economicdata_file<-function(w) {


dialog <- gtkFileChooserDialog("Enter a name for the Economic data series .csv file", BMTmain_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {

COSTS_final <- data.frame(matrix(nrow=(4+length(COSTS_vector)), ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
     COSTS_final[,1] <- c("Units: days, kg, euro, employees", "casestudy.fleetsegmentcode", "casestudy.fishingtechnique", "casestudy.loa", COSTS_vector)     

REVENUES_final <- data.frame(matrix(nrow=length(BMT_SPECIES), ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
REVENUES_final[,1] <- paste("casestudy.revenues.S", 1:length(BMT_SPECIES), sep="")    

REVENUES_discard_final <- data.frame(matrix(nrow=length(BMT_SPECIES), ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
REVENUES_discard_final[,1] <- paste("casestudy.revenues.discard.S", 1:length(BMT_SPECIES), sep="")    

OTHERS_final <- data.frame(matrix(nrow=length(OTHERS_vector), ncol=((length(BMT_YEARS_SIMULATION)*length(BMT_FLEETSEGMENTS)) +1)))
OTHERS_final[,1] <- OTHERS_vector


       for (fl in 1:length(BMT_FLEETSEGMENTS)) {

      COSTS_matrix   <-  ECONOMICDATA_COSTS_list[[fl]] 
     REVENUES_matrix   <-     ECONOMICDATA_REVENUES_list[[fl]]
       
       # !!!!!!!!!!!!!!!!!!!!   # !!!!!!!!!!!!!!!!!!!!   # !!!!!!!!!!!!!!!!!!!!   # !!!!!!!!!!!!!!!!!!!!   
      REVENUES_discard_matrix   <-     ECONOMICDATA_REVENUES_discard_list[[fl]]     # !!!!!!!!!!!!!!!!!!!!
     OTHERS_matrix  <-    ECONOMICDATA_OTHERS_list[[fl]]
       
        COSTS_final[1,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <- BMT_YEARS_SIMULATION
         COSTS_final[c(3:4),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <- ""
         COSTS_final[2,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )]  <-  BMT_FLEETSEGMENTS[fl]
        
        mat <- as.data.frame(t(COSTS_matrix ) , stringsAsFactors =F) 
        COSTS_final[5:(4+length(COSTS_vector)),  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat[2:nrow(mat),]
                   
        mat <- as.data.frame(t(REVENUES_matrix[,-1] ) , stringsAsFactors =F) 
        REVENUES_final[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat
 
         mat <- as.data.frame(t(REVENUES_discard_matrix[,-1] ) , stringsAsFactors =F) 
        REVENUES_discard_final[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat
 
        mat <- as.data.frame(t(OTHERS_matrix[,-1] ) , stringsAsFactors =F) 
        OTHERS_final[,  c( ( (fl-1)*length(BMT_YEARS_SIMULATION)+2 ) : (fl*length(BMT_YEARS_SIMULATION)+1 ) )] <-  mat
  }
     
   ECONOMIC_DATA_final <- data.frame(rbind(COSTS_final, rbind(REVENUES_final, rbind(REVENUES_discard_final, OTHERS_final)))) 
                     
  write.table(ECONOMIC_DATA_final, file=path, sep=";", row.names=F, col.names=F, na = "")

    temp_economic_cfg <-  as.data.frame(mat_cfg_EffortData , stringsAsFactors =F)
    
   levels(temp_economic_cfg[,2]) <- factor(c(levels(temp_economic_cfg[,2]), path))     
 temp_economic_cfg[4,2] <- path

    mat_cfg_EffortData <<- temp_economic_cfg


wnd <- showMessageOK(paste("        fleet segment saved and all economic data exported!        "))

} 
}




bmt_load_economicdata <- function(w) {


dialog <- gtkFileChooserDialog("Select the Economic data series .csv file", BMTmain_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {

   all_OK_values <- c()
         if (path == "" )  {
              all_OK_values <- c(all_OK_values, F)
              showError("Select Economic data series .csv file!")
          } else {
              all_OK_values <- c(all_OK_values, T)
          }

            if (all(all_OK_values)  )  {
             eff_temp <- mat_cfg_EffortData
            levels(eff_temp[,2]) <- factor(c(levels(eff_temp[,2]), path))     
            eff_temp[4,2] <- path 
                    mat_cfg_EffortData  <<- eff_temp   
                    
                    set_economic_data_lists()
      
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_economicdata_fleet_combo)
index_to_update <- which(BMT_FLEETSEGMENTS == selected)       

bmt_economic.COSTS <<- data.frame(ECONOMICDATA_COSTS_list[[index_to_update]],  stringsAsFactors = F)
bmt_economic.REVENUES <<- data.frame(ECONOMICDATA_REVENUES_list[[index_to_update]] ,  stringsAsFactors = F)
bmt_economic.REVENUES_discard <<- data.frame(ECONOMICDATA_REVENUES_discard_list[[index_to_update]] ,  stringsAsFactors = F)
bmt_economic.OTHERS <<- data.frame(ECONOMICDATA_OTHERS_list[[index_to_update]] ,  stringsAsFactors = F)

bmt_reload_COSTS_table()
bmt_reload_REVENUES_table()
bmt_reload_REVENUES_discard_table()
bmt_reload_OTHERS_table()
     }     
}

}