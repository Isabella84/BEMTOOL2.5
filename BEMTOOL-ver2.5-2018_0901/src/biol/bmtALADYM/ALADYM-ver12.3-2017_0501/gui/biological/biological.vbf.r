# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#
#to [years]		
#
#"Male Growth
#K [years-1]"
#
#"Male Growth 
#Linfinity [mm]"

tbl_VBF <- gtkTable(9,12,homogeneous = FALSE)
tbl_VBF$SetRowSpacings(8)
tbl_VBF$SetColSpacings(10)
tbl_VBF$SetBorderWidth(5)

i=0 # column 0
j=0                                                          
tbl_VBF$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
j=j+1
j=j+1
j=j+1
j=j+1
j=j+1
tbl_VBF$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)    

i=i+1 # column 1 
j=0
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("t0 [years]"),i, i+1, j, j+1) 
j=j+1                                                                                                                    
tbl_VBF$Attach(gtkLabel("K [years^-1]"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("Linfinity [mm]"),i, i+1, j, j+1)
j=j+1
j=j+1
j=j+1
tbl_VBF$Attach(gtkLabel("t0 [years]"),i, i+1, j, j+1) 
j=j+1 
tbl_VBF$Attach(gtkLabel("K [years^-1]"),i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(gtkLabel("Linfinity [mm]"),i, i+1, j, j+1)

i=i+1 # column 2
j=0                                                                                                                  
tbl_VBF$Attach(gtkLabel("Lifespan [years]"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1) 
j=j+1                                                                                                                       
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
j=j+1  
j=j+1                                                                                                                 
tbl_VBF$Attach(gtkLabel("Lifespan [years]"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
j=j+1                                                                                                                   
tbl_VBF$Attach(gtkLabel("distribution"),i, i+1, j, j+1)

i=i+1  # column 3

# create objects                                                                                                              
combo_Kdis_M <- gtkComboBoxNewText()
gSignalConnect(combo_Kdis_M, "changed", change_VBFK_M)
for (choice in DISTRIBUTION) {
if (choice != "Gamma")   { 
combo_Kdis_M$appendText(choice)  
}
}

combo_Linfdis_M <- gtkComboBoxNewText()
gSignalConnect(combo_Linfdis_M, "changed", change_VBFLinf_M)    
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_Linfdis_M$appendText(choice)  
}
}

combo_t0dis_M <- gtkComboBoxNewText()
gSignalConnect(combo_t0dis_M, "changed", change_VBFt0_M)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
 combo_t0dis_M$appendText(choice) 
 }
 }

combo_Kdis_F <- gtkComboBoxNewText()
gSignalConnect(combo_Kdis_F, "changed", change_VBFK_F)
for (choice in DISTRIBUTION) {
if (choice != "Gamma")   {
  combo_Kdis_F$appendText(choice) 
  }
   }

combo_Linfdis_F <- gtkComboBoxNewText()
gSignalConnect(combo_Linfdis_F, "changed", change_VBFLinf_F)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
 combo_Linfdis_F$appendText(choice) 
 }
 }

combo_t0dis_F <- gtkComboBoxNewText()
gSignalConnect(combo_t0dis_F, "changed", change_VBFt0_F)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
 combo_t0dis_F$appendText(choice) 
 }
 }
 


entryVBF_M_lifespan <- gtkEntry()

gtkEntrySetWidthChars(entryVBF_M_lifespan, NUMERICAL_ENTRY_LENGTH)
entryVBF_F_lifespan <- gtkEntry()
#gSignalConnect(entryVBF_F_lifespan, "changed", change_fishingM_F)
gtkEntrySetWidthChars(entryVBF_F_lifespan, NUMERICAL_ENTRY_LENGTH) 

gtkEntrySetText(entryVBF_M_lifespan ,biological.lifeSpanM )
gtkEntrySetText(entryVBF_F_lifespan,biological.lifeSpanF ) 

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
#
if (IN_BEMTOOL) {    
gtkEntrySetEditable(entryVBF_M_lifespan, FALSE)
gtkEntrySetEditable(entryVBF_F_lifespan, FALSE)
}
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
#
j=0     
tbl_VBF$Attach(entryVBF_M_lifespan,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(combo_t0dis_M,i, i+1, j, j+1) 
j=j+1        
tbl_VBF$Attach(combo_Kdis_M,i, i+1, j, j+1)
j=j+1     
tbl_VBF$Attach(combo_Linfdis_M,i, i+1, j, j+1)
j=j+1  
j=j+1   
tbl_VBF$Attach(entryVBF_F_lifespan,i, i+1, j, j+1)  
j=j+1 
tbl_VBF$Attach(combo_t0dis_F,i, i+1, j, j+1) 
j=j+1          
tbl_VBF$Attach(combo_Kdis_F,i, i+1, j, j+1)
j=j+1     
tbl_VBF$Attach(combo_Linfdis_F,i, i+1, j, j+1)

i=i+1  # column 4
j=0 
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)
j=j+1 
j=j+1
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("min"),i, i+1, j, j+1)

i=i+1 # column 5

entryVBFtzero_M_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFtzero_M_min, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_M_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_M_min, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_M_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_M_min, NUMERICAL_ENTRY_LENGTH)  

entryVBFtzero_F_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFtzero_F_min, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_F_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_F_min, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_F_min <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_F_min, NUMERICAL_ENTRY_LENGTH)  

j=0
j=j+1 
tbl_VBF$Attach(entryVBFtzero_M_min,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFK_M_min,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFLinf_M_min,i, i+1, j, j+1)
j=j+1
j=j+1 
j=j+1 
tbl_VBF$Attach(entryVBFtzero_F_min,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFK_F_min,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFLinf_F_min,i, i+1, j, j+1)

i=i+1 # column 6
j=0
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)
j=j+1 
j=j+1
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(gtkLabel("max"),i, i+1, j, j+1)

i=i+1   # column 7
entryVBFtzero_M_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFtzero_M_max, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_M_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_M_max, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_M_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_M_max, NUMERICAL_ENTRY_LENGTH)  
entryVBFtzero_F_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFtzero_F_max, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_F_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_F_max, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_F_max <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_F_max, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetText(entryVBFLinf_F_max, "0")
gtkEntrySetText(entryVBFLinf_M_max, "0")

j=0
j=j+1 
tbl_VBF$Attach(entryVBFtzero_M_max,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFK_M_max,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFLinf_M_max,i, i+1, j, j+1)
j=j+1 
j=j+1
j=j+1   
tbl_VBF$Attach(entryVBFtzero_F_max,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFK_F_max,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFLinf_F_max,i, i+1, j, j+1)

i=i+1  # column 8
lbl_A_VBFt0_M <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFt0_M, LABEL_LENGTH)
lbl_A_VBFK_M <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFK_M, LABEL_LENGTH)
lbl_A_VBFLinf_M <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFLinf_M, LABEL_LENGTH) 
lbl_A_VBFt0_F <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFt0_F, LABEL_LENGTH) 
lbl_A_VBFK_F <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFK_F, LABEL_LENGTH)  
lbl_A_VBFLinf_F <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_VBFLinf_F, LABEL_LENGTH) 

j=0
j=j+1
tbl_VBF$Attach(lbl_A_VBFt0_M,i, i+1, j, j+1) 
j=j+1 
tbl_VBF$Attach(lbl_A_VBFK_M,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(lbl_A_VBFLinf_M,i, i+1, j, j+1)
j=j+1 
j=j+1 
j=j+1
tbl_VBF$Attach(lbl_A_VBFt0_F,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(lbl_A_VBFK_F,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(lbl_A_VBFLinf_F,i, i+1, j, j+1)

i=i+1  # column 9
entryVBFt0_M_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFt0_M_a, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_M_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_M_a, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_M_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_M_a, NUMERICAL_ENTRY_LENGTH)  
entryVBFt0_F_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFt0_F_a, NUMERICAL_ENTRY_LENGTH) 
entryVBFK_F_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_F_a, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_F_a <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_F_a, NUMERICAL_ENTRY_LENGTH)

j=0
j=j+1 
tbl_VBF$Attach(entryVBFt0_M_a,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(entryVBFK_M_a,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFLinf_M_a,i, i+1, j, j+1)
j=j+1 
j=j+1 
j=j+1
tbl_VBF$Attach(entryVBFt0_F_a,i, i+1, j, j+1)
j=j+1  
tbl_VBF$Attach(entryVBFK_F_a,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFLinf_F_a,i, i+1, j, j+1)

i=i+1   # column 10
lbl_B_VBFt0_M <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFt0_M, LABEL_LENGTH) 
lbl_B_VBFK_M <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFK_M, LABEL_LENGTH) 
lbl_B_VBFLinf_M <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFLinf_M, LABEL_LENGTH)
lbl_B_VBFt0_F <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFt0_F, LABEL_LENGTH)  
lbl_B_VBFK_F <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFK_F, LABEL_LENGTH) 
lbl_B_VBFLinf_F <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_VBFLinf_F, LABEL_LENGTH)  

j=0
j=j+1 
tbl_VBF$Attach(lbl_B_VBFt0_M,i, i+1, j, j+1)
j=j+1 
tbl_VBF$Attach(lbl_B_VBFK_M,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(lbl_B_VBFLinf_M,i, i+1, j, j+1)
j=j+1 
j=j+1 
j=j+1
tbl_VBF$Attach(lbl_B_VBFt0_F,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(lbl_B_VBFK_F,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(lbl_B_VBFLinf_F,i, i+1, j, j+1)

i=i+1 # column 11
entryVBFt0_M_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFt0_M_b, NUMERICAL_ENTRY_LENGTH)  
entryVBFK_M_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_M_b, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_M_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_M_b, NUMERICAL_ENTRY_LENGTH) 
entryVBFt0_F_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFt0_F_b, NUMERICAL_ENTRY_LENGTH)   
entryVBFK_F_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFK_F_b, NUMERICAL_ENTRY_LENGTH)  
entryVBFLinf_F_b <- gtkEntry()
gtkEntrySetWidthChars(entryVBFLinf_F_b, NUMERICAL_ENTRY_LENGTH) 

j=0
j=j+1
tbl_VBF$Attach(entryVBFt0_M_b,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFK_M_b,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFLinf_M_b,i, i+1, j, j+1) 
j=j+1
j=j+1
j=j+1
tbl_VBF$Attach(entryVBFt0_F_b,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFK_F_b,i, i+1, j, j+1)
j=j+1
tbl_VBF$Attach(entryVBFLinf_F_b,i, i+1, j, j+1)


gtkComboBoxSetActive(combo_Kdis_M, 0 ) 
gtkComboBoxSetActive(combo_Linfdis_M, 0 ) 
gtkComboBoxSetActive(combo_t0dis_M, 0 ) 
gtkComboBoxSetActive(combo_Kdis_F, 0 ) 
gtkComboBoxSetActive(combo_Linfdis_F, 0 ) 
gtkComboBoxSetActive(combo_t0dis_F, 0 ) 