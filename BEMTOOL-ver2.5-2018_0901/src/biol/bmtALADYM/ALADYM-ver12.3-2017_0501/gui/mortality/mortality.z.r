# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




hboxTotalMortality <- gtkHBox(homogeneous = FALSE)

hboxbtn_browse_Zvectorfile <- gtkHBox(FALSE, 5)
btn_browse_Zvectorfile <- gtkButton()
gtkButtonSetLabel(btn_browse_Zvectorfile, "Load Total Mortality...")
btn_browse_Zvectorfile$AddCallback("clicked", select_file_Zvector)
hboxbtn_browse_Zvectorfile$packStart(btn_browse_Zvectorfile, expand = FALSE, fill = FALSE, padding = 10)

btn_browseZvector_export <- gtkButton()
gtkButtonSetLabel(btn_browseZvector_export, "Export Total Mortality...")
btn_browseZvector_export$AddCallback("clicked", save_file_Zvector)
hboxbtn_browse_Zvectorfile$packStart(btn_browseZvector_export, expand = FALSE, fill = FALSE,  padding = 10)

vbox_totalmortality$packStart(hboxbtn_browse_Zvectorfile, expand = FALSE, fill = TRUE,  padding = 10)


hbox_Zseed_M <- gtkHBox(FALSE, 5)

   entry_Zseedvalue_M <- gtkEntry() 
gtkEntrySetWidthChars(entry_Zseedvalue_M, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_Zseedvalue_M, 0)

btn_load_seed_M <- gtkButton()
gtkButtonSetLabel(btn_load_seed_M, "Load seed value")
btn_load_seed_M$AddCallback("clicked", reload_ZvectorM_table)
 
hbox_Zseed_M$packStart(gtkLabel("MALES      Seed value for Z"), expand = F, fill = F,  padding = 10)
hbox_Zseed_M$packStart(entry_Zseedvalue_M, expand = F, fill = F,  padding = 10)
hbox_Zseed_M$packStart(btn_load_seed_M, expand = F, fill = F,  padding = 10) 


# insert table
Zvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
Zvector_M.sw$setShadowType("etched-in")
Zvector_M.sw$setPolicy("automatic", "automatic")
Zvector_M.sw$SetUsize(527, dim_eff_tables)  


#
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
if (IN_BEMTOOL) {
# SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1])   

 if (SAtool != "none") {
if (exists("mortality_temp")) { rm(mortality_temp) }

number_ageclasses <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5]) )
      
Zvector_M_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(Zvector_M_table) <- c("year","seed", MONTHS)
Zvector_M_table$year <- years
Zvector_M_table$seed <- ""
for (ye in 1:simperiod) {    
if (ye == 1) {
      # Zvector_M_table$seed[1] <- mean( as.numeric(as.character(VITinfo[[ALADYM_spe]][[ye]]$results[[1]]$VPA_results_mortalities[c(1:number_ageclasses),2])) )
       Zvector_M_table$seed[1] <- Interactionsyear[[ye]][[ALADYM_spe]]@mortalities$Z[(length(BMT_FLEETSEGMENTS)+1)]
}
 #Zvector_M_table[ye,3:ncol(Zvector_M_table)] <-   mean( as.numeric(as.character(VITinfo[[ALADYM_spe]][[ye]]$results[[1]]$VPA_results_mortalities[c(1:number_ageclasses),2])) )
 Zvector_M_table[ye,3:ncol(Zvector_M_table)] <- Interactionsyear[[ye]][[ALADYM_spe]]@mortalities$Z[(length(BMT_FLEETSEGMENTS)+1)]
} 
mortality.Zvector.males <<- Zvector_M_table[,c(1,3:14)]  
 gtkEntrySetText(entry_Zseedvalue_M, Zvector_M_table$seed[1])

 }
}
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
#




Zvector_M <<- list()
Zvector_MIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
Zvector_M.create_model()
# create tree view
Zvector_M.treeview <<- gtkTreeViewNewWithModel(Zvector_M.model)
Zvector_M.treeview$setRulesHint(TRUE)
Zvector_M.treeview$getSelection()$setMode("single")
Zvector_M.add_columns(Zvector_M.treeview)
Zvector_M.sw$add(Zvector_M.treeview) 
vboxZtable_males <- gtkHBox(FALSE, 5)       
vboxZtable_males$packStart(Zvector_M.sw , TRUE, TRUE)   


# ************************************************************************ Z for FEMALES

hbox_Zseed_F <- gtkHBox(FALSE, 5)

 entry_Zseedvalue_F <- gtkEntry() 
gtkEntrySetWidthChars(entry_Zseedvalue_F, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetText(entry_Zseedvalue_F, 0) 

 btn_load_seed_F <- gtkButton()
gtkButtonSetLabel(btn_load_seed_F, "Load seed value")
btn_load_seed_F$AddCallback("clicked", reload_ZvectorF_table)

hbox_Zseed_F$packStart(gtkLabel("FEMALES      Seed value for Z") , F, F, padding=10)   
 hbox_Zseed_F$packStart(entry_Zseedvalue_F , F, F, padding=10)  
  hbox_Zseed_F$packStart(btn_load_seed_F , F, F, padding=10)    

# insert table
Zvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
Zvector_F.sw$setShadowType("etched-in")
Zvector_F.sw$setPolicy("automatic", "automatic")
Zvector_F.sw$SetUsize(527, dim_eff_tables)  


## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
#
if (IN_BEMTOOL) {
 if (SAtool != "none") {
 gtkEntrySetText(entry_Zseedvalue_F, Zvector_M_table$seed[1])
mortality.Zvector.females <<- Zvector_M_table[,c(1,3:14)] 
  }
} 
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------



Zvector_F <<- list()
Zvector_FIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
Zvector_F.create_model()
# create tree view
Zvector_F.treeview <<- gtkTreeViewNewWithModel(Zvector_F.model)
Zvector_F.treeview$setRulesHint(TRUE)
Zvector_F.treeview$getSelection()$setMode("single")
Zvector_F.add_columns(Zvector_F.treeview)
Zvector_F.sw$add(Zvector_F.treeview)
vboxZtable_females <- gtkHBox(FALSE, 5)    
vboxZtable_females$packStart(Zvector_F.sw , TRUE, TRUE)   



# set the final table Z
tbl_TotalMortality <- gtkTable(2,2,homogeneous = FALSE)

tbl_TotalMortality$SetRowSpacings(7)
tbl_TotalMortality$SetColSpacings(25)
tbl_TotalMortality$SetBorderWidth(5)

 i=0    # column 1
 j=0
 tbl_TotalMortality$Attach(hbox_Zseed_M,i, i+1, j, j+1)   
 j=j+1
 tbl_TotalMortality$Attach(vboxZtable_males,i, i+1, j, j+1)    

 i=i+1   # column 2
 j=0
 tbl_TotalMortality$Attach(hbox_Zseed_F,i, i+1, j, j+1)      
 j=j+1
 tbl_TotalMortality$Attach(vboxZtable_females,i, i+1, j, j+1)   
 
 hboxTotalMortality$packStart(tbl_TotalMortality, expand = T, fill = T, padding=0) 