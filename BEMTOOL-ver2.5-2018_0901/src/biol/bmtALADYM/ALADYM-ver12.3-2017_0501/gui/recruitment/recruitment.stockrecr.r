# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vboxSRrelationship <- gtkVBox(FALSE, 5)
			
hboxSRType <- gtkHBox(FALSE, 5)
hboxSRType$packStart(gtkLabel("Type of SR relationship"), expand = FALSE, fill = FALSE, padding = 5)
combo_SRtype <- gtkComboBoxNewText()
gSignalConnect(combo_SRtype, "changed", deactivate_SR_unused_params)
for (choice in SR_TYPE) { combo_SRtype$appendText(choice) }



#combo_SRtype$setPopdownStrings(SR_TYPE)
hboxSRType$packStart(combo_SRtype, expand = FALSE, fill = FALSE, padding = 5)

hboxSR_params <- gtkHBox(FALSE, 5)
lbl_SRparameters <- gtkLabel("SR relationship parameters")
hboxSR_params$packStart(lbl_SRparameters, expand = FALSE, fill = FALSE, padding = 5)
lbl_SR_params_a <- gtkLabel("a")
hboxSR_params$packStart(lbl_SR_params_a, expand = FALSE, fill = FALSE, padding = 5)
entrySR_params_a <- gtkEntry()
gtkEntrySetWidthChars(entrySR_params_a, NUMERICAL_ENTRY_LENGTH)
hboxSR_params$packStart(entrySR_params_a, expand = FALSE, fill = FALSE, padding = 5)
lbl_SR_params_b <- gtkLabel("b")
hboxSR_params$packStart(lbl_SR_params_b, expand = FALSE, fill = FALSE, padding = 5)
entrySR_params_b <- gtkEntry()
gtkEntrySetWidthChars(entrySR_params_b, NUMERICAL_ENTRY_LENGTH)
hboxSR_params$packStart(entrySR_params_b, expand = FALSE, fill = FALSE, padding = 5)
lbl_SR_params_c <- gtkLabel("c")
hboxSR_params$packStart(lbl_SR_params_c, expand = FALSE, fill = FALSE, padding = 5)
entrySR_params_c <- gtkEntry()
gtkEntrySetWidthChars(entrySR_params_c, NUMERICAL_ENTRY_LENGTH)
hboxSR_params$packStart(entrySR_params_c, expand = FALSE, fill = FALSE, padding = 5)

vboxSRrelationship$packStart(hboxSRType, expand = FALSE, fill = FALSE, padding = 5)

hboxSRType$packStart(hboxSR_params, expand = FALSE, fill = FALSE, padding = 5)
#vboxSRrelationship$packStart(hboxSR_params, expand = FALSE, fill = FALSE, padding = 5)



#  # column 3
#hboxSRvector <- gtkHBox(FALSE, 5)
#lbl_SR_Rfile <<- gtkLabel("Load values of recruitment from .csv file")
#hboxSRvector$packStart(lbl_SR_Rfile, expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse_Rfile <<- gtkButton()
#gtkButtonSetLabel(btn_browse_Rfile, "Browse...")
#btn_browse_Rfile$AddCallback("clicked", select_file_StockRecr_vector)
#hboxSRvector$packStart(btn_browse_Rfile, expand = FALSE, fill = FALSE, padding = 5)
#lbl_SRvectorFile <<- gtkLabel("C:\\ ")
#hboxSRvector$packStart(lbl_SRvectorFile, expand = FALSE, fill = FALSE, padding = 5)

# vboxSRrelationship$packStart(hboxSRvector, expand = FALSE, fill = FALSE, padding =10)

hboxOFFSPRING <- gtkHBox(FALSE, 5)
lbl_OFFSPRING_seed_value <- gtkLabel("Seed value for recruitment")
hboxOFFSPRING$packStart(lbl_OFFSPRING_seed_value, expand = FALSE, fill = FALSE, padding = 5)  
entry_OFFSPRING_seedvalue <- gtkEntry()  
gtkEntrySetWidthChars(entry_OFFSPRING_seedvalue, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_OFFSPRING_seedvalue, 0)
hboxOFFSPRING$packStart(entry_OFFSPRING_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_recruitment <- gtkButton()
gtkButtonSetLabel(btn_load_seed_recruitment, "Load seed value")
btn_load_seed_recruitment$AddCallback("clicked", reload_recruitment_table)
hboxOFFSPRING$packStart(btn_load_seed_recruitment, expand = FALSE, fill = FALSE, padding = 5) 

btn_browse_Rfile <<- gtkButton()
gtkButtonSetLabel(btn_browse_Rfile, "Load recruitment...")
btn_browse_Rfile$AddCallback("clicked", select_file_StockRecr_vector)
hboxOFFSPRING$packStart(btn_browse_Rfile, expand = FALSE, fill = FALSE, padding = 5)

btn_browse_save_recruitment <- gtkButton()
gtkButtonSetLabel(btn_browse_save_recruitment, "Export recruitment...")
btn_browse_save_recruitment$AddCallback("clicked", save_file_recruitment_vector)
hboxOFFSPRING$packStart(btn_browse_save_recruitment, expand = FALSE, fill = FALSE, padding = 5)


vboxSRrelationship$packStart(hboxOFFSPRING, expand = FALSE, fill = FALSE, padding = 0)


## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
#
if (IN_BEMTOOL) {
 SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1])
#
if (SAtool != "none") {
if (exists("recruitment_temp")) { rm(recruitment_temp) }
#
#
if (SAtool == "VIT") {
     ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5])) #+1 
     ages_M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),6])) #+1
     # ages_F is taken into account as in this version only combined sex is implemented
      VIT.sex <-	as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),2])   
     if (exists("ald_recruits_vector")) { rm(ald_recruits_vector) }
#      
     for (ye in 1:simperiod) {
#     
  if (missing_SA[ALADYM_spe,ye] == "Y") {
#     
          # BAS	MZ_estimated, FZ_estimated
          # mean of Z (from VIT, the annual values have to be repeated for all the months) considering the elements between age = minage and age = maxage-1 by sex (in ALADYM). The age are expressed in years
          # mean_Z_overages <- mean( as.numeric(as.character(VITinfo[[ALADYM_spe]][[ye]]$results[[1]]$VPA_results_mortalities[c(1:ages_F),2])) )
            #if (!exists("ald_Z_vector")) {
             #  ald_Z_vector <- c(mean_Z_overages, rep(mean_Z_overages, length(MONTHS)))
          #  } else {
           #    ald_Z_vector <- c(ald_Z_vector, rep(mean_Z_overages, length(MONTHS)))
            #}
          #INP	Recruits 	thousands	only the first age class has to be considered by year (values have to be repeated for all the months);  to be divided by 1000   
#
                if (!VIT.sex) {
            initial_number <-  as.numeric(as.character( VITinfo[[ALADYM_spe]][[ye]]$results[[1]]$VPA_results_nb[1,2]  ))
                } else {
            initial_number <-  as.numeric(as.character( VITinfo[[ALADYM_spe]][[ye]]$results[[1]]$VPA_results_nb[1,2]  ))   + as.numeric(as.character( VITinfo[[ALADYM_spe]][[ye]]$results[[2]]$VPA_results_nb[1,2]  ))
                }     
                 
    } else {
         initial_number <- NA
         # no assessment
    }
           if (!exists("ald_recruits_vector")) {
               ald_recruits_vector <- c(initial_number, rep(initial_number, length(MONTHS)))
            } else {
               ald_recruits_vector <- c(ald_recruits_vector, rep(initial_number, length(MONTHS)))
            } 

#    
   } 
          ald_recruits_vector <- ald_recruits_vector / 1000   
      # setting of XSA values
     }  else if (SAtool == "XSA") {  
     ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5])) # + 1
     xsa_harvest <- data.frame(XSAinfo[[ALADYM_spe]]$results$results@harvest@.Data)  
     xsa_m <- data.frame(XSAinfo[[ALADYM_spe]]$results$results@m@.Data)
     xsa_z_vector <- xsa_harvest + xsa_m
     xsa_recruits <-  data.frame(XSAinfo[[ALADYM_spe]]$results$results@stock.n@.Data)[1,]
    # if (exists("ald_Z_vector") ) { rm(ald_Z_vector) }
     if (exists("ald_recruits_vector")) { rm(ald_recruits_vector) } 
      ye_ord <- 0
      for (ye in 1:simperiod) {
     #   mean_Z_overages <- mean(  xsa_z_vector[c(1:ages_F),ye] )                                         
      #      if (!exists("ald_Z_vector")) {
       #        ald_Z_vector <- c(mean_Z_overages, rep(mean_Z_overages, length(MONTHS)))
        #    } else {
         #      ald_Z_vector <- c(ald_Z_vector, rep(mean_Z_overages, length(MONTHS)))
          #  }  
      # INP	Recruits 	thousands	only the first age class has to be considered by year (values have to be repeated for all the months)

      
           if (!exists("ald_recruits_vector")) {
            if (missing_SA[ALADYM_spe,ye] == "Y") {
             ye_ord <- ye_ord + 1
               ald_recruits_vector <- c(as.numeric(as.character(xsa_recruits[ye_ord])), rep(as.numeric(as.character(xsa_recruits[ye_ord])), length(MONTHS)))
               } else {
               # no assessment
               ald_recruits_vector <- rep(NA, (length(MONTHS)+1)) 
               }
            } else {
             if (missing_SA[ALADYM_spe,ye] == "Y") {
               ye_ord <- ye_ord + 1
               ald_recruits_vector <- c(ald_recruits_vector, rep(as.numeric(as.character(xsa_recruits[ye_ord])), length(MONTHS)))
               } else {
               # no assessment
               ald_recruits_vector <- c(ald_recruits_vector, rep(NA, length(MONTHS)))
               }
            } 
      
      }
     } else if (SAtool == "from Report") {  
     ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5])) # + 1
     #xsa_harvest <- data.frame(XSAinfo[[ALADYM_spe]]$results$results@harvest@.Data)  
     #xsa_m <- data.frame(XSAinfo[[ALADYM_spe]]$results$results@m@.Data)
     #xsa_z_vector <- xsa_harvest + xsa_m
     report_recruits <-  ReportINFO[[ALADYM_spe]]$results$stock_nb[1,]
     #if (exists("ald_Z_vector") ) { rm(ald_Z_vector) }
     if (exists("ald_recruits_vector")) { rm(ald_recruits_vector) }
       ye_ord <- 0 
      for (ye in 1:simperiod) {
        #mean_Z_overages <- mean(  xsa_z_vector[c(1:ages_F),ye] )                                         
         #   if (!exists("ald_Z_vector")) {
          #     ald_Z_vector <- c(mean_Z_overages, rep(mean_Z_overages, length(MONTHS)))
           # } else {
            #   ald_Z_vector <- c(ald_Z_vector, rep(mean_Z_overages, length(MONTHS)))
           # }  
      # INP	Recruits 	thousands	only the first age class has to be considered by year (values have to be repeated for all the months)

           if (!exists("ald_recruits_vector")) {
            if (missing_SA[ALADYM_spe,ye] == "Y") {
                ye_ord <- ye_ord + 1
               ald_recruits_vector <- c(as.numeric(as.character(report_recruits[1,ye_ord])), rep(as.numeric(as.character(report_recruits[1,ye_ord])), length(MONTHS)))
               }else {
               #no assessment
                ald_recruits_vector <- rep(NA, (length(MONTHS)+1))
               }
            } else {
             if (missing_SA[ALADYM_spe,ye] == "Y") {
                    ye_ord <- ye_ord + 1
               ald_recruits_vector <- c(ald_recruits_vector, rep(as.numeric(as.character(report_recruits[1,ye_ord])), length(MONTHS)))
               } else {
               # no assessment
               ald_recruits_vector <- c(ald_recruits_vector, rep(NA, length(MONTHS)))
               }
            } 
      
      }
     } else if (SAtool == "SURBA") {
     ages_C <- length(SURBAinfo[[ALADYM_spe]]$results$age_classes)
     surba_recruits <-  SURBAinfo[[ALADYM_spe]]$results$recruitment[,5]
     #if (exists("ald_Z_vector") ) { rm(ald_Z_vector) }
     if (exists("ald_recruits_vector")) { rm(ald_recruits_vector) } 
      for (ye in 1:simperiod) {
        #mean_Z_overages <- mean(  xsa_z_vector[c(1:ages_F),ye] )                                         
         #   if (!exists("ald_Z_vector")) {
          #     ald_Z_vector <- c(mean_Z_overages, rep(mean_Z_overages, length(MONTHS)))
           # } else {
            #   ald_Z_vector <- c(ald_Z_vector, rep(mean_Z_overages, length(MONTHS)))
           # }  
      # INP	Recruits 	thousands	only the first age class has to be considered by year (values have to be repeated for all the months)

           if (!exists("ald_recruits_vector")) {
              if (missing_SA[ALADYM_spe,ye] == "Y") {
               ald_recruits_vector <- c(as.numeric(as.character(surba_recruits[ye])), rep(as.numeric(as.character(surba_recruits[ye])), length(MONTHS)))
               } else {
               # no assessment
                ald_recruits_vector <- rep(NA, (length(MONTHS)+1))
               }
            } else {
             if (missing_SA[ALADYM_spe,ye] == "Y") {
               ald_recruits_vector <- c(ald_recruits_vector, rep(as.numeric(as.character(surba_recruits[ye])), length(MONTHS)))
               } else {
               # no assessment
                ald_recruits_vector <- c(ald_recruits_vector, rep(NA, length(MONTHS)))
               }
            } 
      
      }
     }
     
     
# number_ageclasses <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5]) )
      
recruitment_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(recruitment_table) <- c("year","seed", MONTHS)
recruitment_table$year <- years
recruitment_table$seed <- ""
for (ye in 1:simperiod) {    
if (ye == 1) {
       recruitment_table$seed[1] <- ald_recruits_vector[1]
}
      # print(c( ((ye-1)*12+2) : (ye*12+1)))
       recruitment_table[ye,3:ncol(recruitment_table)] <- ald_recruits_vector[c( ((ye-1)*12+2) : (ye*12+1))]
} 
stockrecruitment.SRvector <<- recruitment_table[,c(1,3:14)]  
gtkEntrySetText(entry_OFFSPRING_seedvalue, recruitment_table$seed[1])
 }

}

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
#

recruitment.sw <<- gtkScrolledWindowNew(NULL, NULL)
recruitment.sw$setShadowType("etched-in")
recruitment.sw$setPolicy("automatic", "automatic")
recruitment.sw$SetUsize(100, dim_eff_tables)  
recruitments <<- list()
recruitmentIndex <<- 0
# ------------------------------
# create model
recruitments.create_model()
# create tree view
recruitments.treeview <<- gtkTreeViewNewWithModel(recruitments.model)
recruitments.treeview$setRulesHint(TRUE)
recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns(recruitments.treeview)
recruitment.sw$add(recruitments.treeview)    
vboxSRrelationship$packStart(recruitment.sw , TRUE, TRUE, 0) 

hboxStockRfile_save <- gtkHBox(FALSE, 5)
btn_browse_save_recruitment <- gtkButton()
gtkButtonSetLabel(btn_browse_save_recruitment, "Save recruitment...")
btn_browse_save_recruitment$AddCallback("clicked", save_file_recruitment_vector)
hboxStockRfile_save$packStart(btn_browse_save_recruitment, expand = FALSE, fill = FALSE, padding = 0)
#vboxSRrelationship$packStart(hboxStockRfile_save, expand = FALSE, fill = FALSE, padding = 0)
#



hboxUnitSpawners <- gtkHBox(FALSE, 5)

## Create a radio button with a GtkEntry widget 
radio_tons <- gtkRadioButton()
radio_tons$add(gtkLabel("Tons (biomass)"))

radio_thousands <- gtkRadioButtonNewWithLabelFromWidget(radio_tons, "Thousands (numbers)")
  
tbl_unitSpawners <- gtkTable(1,3,homogeneous = FALSE)
tbl_unitSpawners$SetRowSpacings(7)
tbl_unitSpawners$SetColSpacings(30)
tbl_unitSpawners$SetBorderWidth(5)

i=0
j=0   
tbl_unitSpawners$Attach(gtkLabel("Unit for spawners"),i, i+1, j, j+1)  

i=i+1
j=0   
tbl_unitSpawners$Attach(radio_tons,i, i+1, j, j+1)  

i=i+1
j=0   
tbl_unitSpawners$Attach(radio_thousands,i, i+1, j, j+1)  


hboxUnitSpawners$packStart(tbl_unitSpawners, FALSE, FALSE, 5) 
    
vboxSRrelationship$PackStart(hboxUnitSpawners, expand = FALSE, fill = FALSE, padding = 5)

hboxUnitSpawners <- gtkHBox(FALSE, 5)
hboxUnitSpawners$packStart(gtkLabel("Unit for spawners"), expand = FALSE, fill = FALSE, padding = 5)

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
if (IN_BEMTOOL) {
gtkComboBoxSetActive(combo_SRtype, (which(SR_TYPE == "from vector")-1) )
} else {
#
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
      gtkComboBoxSetActive(combo_SRtype, 3)
      }