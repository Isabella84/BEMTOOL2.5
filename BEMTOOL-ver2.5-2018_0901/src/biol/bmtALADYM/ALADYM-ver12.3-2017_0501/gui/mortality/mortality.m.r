# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 # COSTRUZIONE OGGETTI 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
 
 
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FINESTRE CON DATI 

Mvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
Mvector_M.sw$setShadowType("etched-in")
Mvector_M.sw$setPolicy("automatic", "automatic")
Mvector_M.sw$SetUsize(100, dim_eff_tables) 

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
if (IN_BEMTOOL) {
       SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1])
      if (SAtool != "none") {
#      
if (is.na(Populations[[ALADYM_spe]]@M.cost$M[1]) ) {
#
for (nr in 1:nrow(Populations[[ALADYM_spe]]@M.vect$M)) {
   if (nr==1 ) {
       mortality_temp_M <- as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$M[nr,] ))
   } else {
       mortality_temp_M <- c(mortality_temp_M, as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$M[nr,] )) )
   }
}
all_mortality <-  c(mortality_temp_M[1], mortality_temp_M)

mortality_dataframe_M <- data.frame(cbind(seq(Tr, length(mortality_temp_M), 1), all_mortality[(Tr+1):(length(all_mortality))]) )
colnames(mortality_dataframe_M) <- c("age_month",	"M")
mortality.Mvector.males <<- mortality_dataframe_M 

mortality.Mvector.males <<- mortality.Mvector.males[which(as.numeric(as.character(mortality.Mvector.males$age_month)) >= Tr),]

}
}
}
# ---------------------------------------------------------------------------
# --------------------------------------------------------------------------- 
Mvector_M <<- list()
Mvector_MIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
Mvector_M.create_model()
# create tree view
Mvector_M.treeview <<- gtkTreeViewNewWithModel(Mvector_M.model)
Mvector_M.treeview$setRulesHint(TRUE)
Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns(Mvector_M.treeview)
Mvector_M.sw$add(Mvector_M.treeview)   


# **************************************************************************************

Mvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
Mvector_F.sw$setShadowType("etched-in")
Mvector_F.sw$setPolicy("automatic", "automatic")
Mvector_F.sw$SetUsize(100, dim_eff_tables)  


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
if (IN_BEMTOOL) {
 if (exists("mortality_temp_F")) { rm(mortality_temp_F) }
 if (SAtool != "none") {
if (is.na(Populations[[ALADYM_spe]]@M.cost$M[2]) ) {

for (nr in 1:nrow(Populations[[ALADYM_spe]]@M.vect$F)) {
   if (nr==1 ) {
       mortality_temp_F <- as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$F[nr,] ))
   } else {
       mortality_temp_F <- c(mortality_temp_F, as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$F[nr,] )) )
   }
}

all_mortality <-  c(mortality_temp_F[1], mortality_temp_F)

mortality_dataframe_F <- data.frame(cbind(seq(Tr, length(mortality_temp_F), 1), all_mortality[(Tr+1):(length(all_mortality))] ) )
colnames(mortality_dataframe_F) <- c("age_month",	"M")
mortality.Mvector.females <<- mortality_dataframe_F 
#
mortality.Mvector.females <<- mortality.Mvector.females[which(as.numeric(as.character(mortality.Mvector.females$age_month)) >= Tr),]
#
}
}
}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

Mvector_F <<- list()
Mvector_FIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
Mvector_F.create_model()
# create tree view
Mvector_F.treeview <<- gtkTreeViewNewWithModel(Mvector_F.model)
Mvector_F.treeview$setRulesHint(TRUE)
Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns(Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)    

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   CHECKS 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   COMBO 

combo_Mtype_M <- gtkComboBoxNewText()
for (choice in MORTALITY_TYPE) { combo_Mtype_M$appendText(choice) }

combo_Mtype_F <- gtkComboBoxNewText()
for (choice in MORTALITY_TYPE) {  combo_Mtype_F$appendText(choice)  }

gSignalConnect(combo_Mtype_M, "changed", deactivate_M_unused_params_M)
gSignalConnect(combo_Mtype_F, "changed", deactivate_M_unused_params_F)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON 
   
btn_browse_Msave_M <- gtkButton()
btn_browse_Msave_F <- gtkButton()
btn_browse_Mvectorfile_M <- gtkButton()
btn_browse_Mvectorfile_F <- gtkButton()

gtkButtonSetLabel(btn_browse_Msave_M, "Export M...")
gtkButtonSetLabel(btn_browse_Msave_F, "Export M...")
gtkButtonSetLabel(btn_browse_Mvectorfile_M, "Load M...")
gtkButtonSetLabel(btn_browse_Mvectorfile_F, "Load M...")


btn_browse_Msave_M$AddCallback("clicked", save_file_Mvector_M)
btn_browse_Msave_F$AddCallback("clicked", save_file_Mvector_F) 
btn_browse_Mvectorfile_M$AddCallback("clicked", select_file_MvectorM)
btn_browse_Mvectorfile_F$AddCallback("clicked", select_file_MvectorF)  


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL 
 lbl_Mconstant_M <- gtkLabel("Constant")
  lbl_Mvectorfile_M <- gtkLabel("Load M vector from .csv file")
  lbl_Mconstant_F <- gtkLabel("Constant")
  lbl_Mvectorfile_F <- gtkLabel("Load M vector from .csv file")
  
  gtkLabelSetWidthChars(lbl_Mconstant_M, LABEL_LENGTH) 
  gtkLabelSetWidthChars(lbl_Mconstant_F, LABEL_LENGTH) 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY 
 entryMconstant_M <- gtkEntry()
 entryMconstant_F <- gtkEntry()

gtkEntrySetWidthChars(entryMconstant_M, 6) 
gtkEntrySetWidthChars(entryMconstant_F, 6) 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX 

 hboxNaturalMortality <- gtkHBox(homogeneous = FALSE)
  hboxMvector_M <- gtkHBox(FALSE, 5)
  hboxMvector_Mfile_save <- gtkHBox(FALSE, 5)
  hboxMvector_F <- gtkHBox(FALSE, 5)
  hboxMvector_Ffile_save <- gtkHBox(FALSE, 5)
  
  
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX 
 vboxM_M <- gtkVBox(homogeneous = FALSE)
vboxM_F <- gtkVBox(homogeneous = FALSE)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 

tbl_NaturalM_M <- gtkTable(1,5,homogeneous = FALSE)
tbl_NaturalM_F <- gtkTable(1,5,homogeneous = FALSE)
tbl_Nat_Mortality <- gtkTable(2,2,homogeneous = FALSE)


# ----------------------------------------------------  tbl_NaturalM_M
 tbl_NaturalM_M$SetRowSpacings(7)
 tbl_NaturalM_M$SetColSpacings(15)
 tbl_NaturalM_M$SetBorderWidth(5)
 
j=0  	# R0 '''''''''''''''''''
i=0  # C0  -------------------------------------------
tbl_NaturalM_M$Attach(gtkLabel("MALES       M type"),i, i+1,j, j+1)
i=i+1 # C1  -------------------------------------------
tbl_NaturalM_M$Attach(combo_Mtype_M,i, i+1,j, j+1)
i=i+1  # C2  -------------------------------------------
tbl_NaturalM_M$Attach(btn_browse_Mvectorfile_M,i, i+1,j, j+1)
i=i+1  # C3  -------------------------------------------
tbl_NaturalM_M$Attach(btn_browse_Msave_M,i, i+1, j, j+1)    
i=i+1  # C4  -------------------------------------------
tbl_NaturalM_M$Attach(lbl_Mconstant_M,i, i+1, j, j+1)
i=i+1 # C5  -------------------------------------------
tbl_NaturalM_M$Attach(entryMconstant_M,i, i+1,j, j+1)


# ----------------------------------------------------  tbl_NaturalM_F

 tbl_NaturalM_F$SetRowSpacings(7)
 tbl_NaturalM_F$SetColSpacings(15)
 tbl_NaturalM_F$SetBorderWidth(5)

j=0  	# R0 ''''''''''''''''''' 
i=0  # C0  -------------------------------------------
 tbl_NaturalM_F$Attach(gtkLabel("FEMALES      M type"),i, i+1, j, j+1)    
i=i+1  # C1  -------------------------------------------
tbl_NaturalM_F$Attach(combo_Mtype_F,i, i+1, j, j+1)
i=i+1   # C2  -------------------------------------------
tbl_NaturalM_F$Attach(btn_browse_Mvectorfile_F,i, i+1, j, j+1)
i=i+1   # C3  -------------------------------------------
tbl_NaturalM_F$Attach(btn_browse_Msave_F,i, i+1, j, j+1)
i=i+1  # C4  -------------------------------------------
tbl_NaturalM_F$Attach(lbl_Mconstant_F,i, i+1,j, j+1)    
 i=i+1  # C5  -------------------------------------------
tbl_NaturalM_F$Attach(entryMconstant_F,i, i+1, j, j+1)


# ----------------------------------------------------  tbl_Nat_Mortality

 tbl_Nat_Mortality$SetRowSpacings(7)
 tbl_Nat_Mortality$SetColSpacings(25)
 tbl_Nat_Mortality$SetBorderWidth(5)

 j=0  	# R0 ''''''''''''''''''' 
i=0  # C0  -------------------------------------------
 tbl_Nat_Mortality$Attach(tbl_NaturalM_M,i, i+1, 1, 2)
 tbl_Nat_Mortality$Attach(vboxM_M,i, i+1, 2, 3)    
 i=i+1  # C1  -------------------------------------------
 tbl_Nat_Mortality$Attach(tbl_NaturalM_F,i, i+1, 1, 2)
 tbl_Nat_Mortality$Attach(vboxM_F,i, i+1, 2, 3)    



 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같



# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
		
hboxMvector_M$packStart(btn_browse_Mvectorfile_M, expand = FALSE, fill = FALSE, padding = 5)
vboxM_M$packStart(hboxMvector_M, expand = FALSE, fill = FALSE,padding = 5)
vboxM_M$packStart(Mvector_M.sw , TRUE, TRUE, padding = 0)   
hboxMvector_M$packStart(btn_browse_Msave_M, expand = FALSE, fill = FALSE, padding = 5)
hboxMvector_F$packStart(btn_browse_Mvectorfile_F, expand = FALSE, fill = FALSE, padding = 5)
vboxM_F$packStart(hboxMvector_F, expand = FALSE, fill = FALSE, padding=5)     
vboxM_F$packStart(Mvector_F.sw , TRUE, TRUE, padding = 0)   
hboxMvector_F$packStart(btn_browse_Msave_F, expand = FALSE, fill = FALSE, padding = 5)
hboxNaturalMortality$packStart(tbl_Nat_Mortality, expand = T, fill = F, padding=0)  # , padding=5


   
   # ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
	
	
	# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
if (IN_BEMTOOL) {
 if (SAtool != "none") {
if (!is.na(Populations[[ALADYM_spe]]@M.cost$M[1]) ) {
    gtkComboBoxSetActive(combo_Mtype_M, (which(MORTALITY_TYPE == "M constant")-1) )
    gtkEntrySetText(entryMconstant_M, Populations[[ALADYM_spe]]@M.cost$M[1] )
    gtkWidgetSetSensitive(btn_browse_Mvectorfile_M, FALSE)
    gtkWidgetSetSensitive(Mvector_M.treeview, FALSE)
    gtkWidgetSetSensitive(  btn_browse_Msave_M, FALSE) 
} else {
    gtkComboBoxSetActive(combo_Mtype_M, (which(MORTALITY_TYPE == "From vector")-1) )
    gtkWidgetSetSensitive(lbl_Mconstant_M, FALSE)
    gtkWidgetSetSensitive(entryMconstant_M, FALSE)
}
} else {
    gtkComboBoxSetActive(combo_Mtype_M, (which(MORTALITY_TYPE == "From vector")-1) )
    gtkWidgetSetSensitive(lbl_Mconstant_M, FALSE)
    gtkWidgetSetSensitive(entryMconstant_M, FALSE)
}
}

if (IN_BEMTOOL) {
if (SAtool != "none") {
if (!is.na(Populations[[ALADYM_spe]]@M.cost$M[2]) ) {
    gtkComboBoxSetActive(combo_Mtype_F, (which(MORTALITY_TYPE == "M constant")-1) )
    gtkEntrySetText(entryMconstant_F, Populations[[ALADYM_spe]]@M.cost$M[2] )
    gtkWidgetSetSensitive(btn_browse_Mvectorfile_F, FALSE)
    gtkWidgetSetSensitive(Mvector_F.treeview, FALSE)
    gtkWidgetSetSensitive(btn_browse_Msave_F, FALSE) 
} else {
    gtkComboBoxSetActive(combo_Mtype_F, (which(MORTALITY_TYPE == "From vector")-1) )
    gtkWidgetSetSensitive(lbl_Mconstant_F, FALSE)
    gtkWidgetSetSensitive(entryMconstant_F, FALSE)
}
} else {
    gtkComboBoxSetActive(combo_Mtype_F, (which(MORTALITY_TYPE == "From vector")-1) )
    gtkWidgetSetSensitive(lbl_Mconstant_F, FALSE)
    gtkWidgetSetSensitive(entryMconstant_F, FALSE)
}
} else {
gtkComboBoxSetActive(combo_Mtype_M, 2)
gtkComboBoxSetActive(combo_Mtype_F, 2)

   }
   
   
   
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------