# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






tbl_AB <- gtkTable(2,5,homogeneous = FALSE)
tbl_AB$SetRowSpacings(7)
tbl_AB$SetColSpacings(30)
tbl_AB$SetBorderWidth(5)

i=0  # column 1 
j=0   
tbl_AB$Attach(gtkLabel("MALES"),i, i+1, j, j+1) 
j=j+1 
tbl_AB$Attach(gtkLabel("FEMALES"),i, i+1,  j, j+1)

i=i+1  # column 2 
 lbl_a_male <- gtkLabel("a [g/mm^b]")
 lbl_a_female <- gtkLabel("a [g/mm^b]")
 gtkLabelSetWidthChars(lbl_a_male, 15) 
 gtkLabelSetWidthChars(lbl_a_female, 15) 
 
j=0    
tbl_AB$Attach(lbl_a_male,i, i+1,  j, j+1)  
j=j+1
tbl_AB$Attach(lbl_a_female,i, i+1, j, j+1)

i=i+1  # column 3 
entryAB_A_M <- gtkEntry()
gtkEntrySetWidthChars(entryAB_A_M, NUMERICAL_ENTRY_LENGTH)
entryAB_A_F <- gtkEntry() 
gtkEntrySetWidthChars(entryAB_A_F, NUMERICAL_ENTRY_LENGTH) 

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
if (IN_BEMTOOL) {
gtkEntrySetText(entryAB_A_M, Populations[[ALADYM_spe]]@lengthweight$a[1] )
gtkEntrySetText(entryAB_A_F, Populations[[ALADYM_spe]]@lengthweight$a[2] )
gtkEntrySetEditable(entryAB_A_M, FALSE)
gtkEntrySetEditable(entryAB_A_F, FALSE)
}
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------

j=0 
tbl_AB$Attach(entryAB_A_M,i, i+1, j, j+1)
j=j+1  
tbl_AB$Attach(entryAB_A_F,i, i+1, j, j+1)

i=i+1  # column 4 
 lbl_b_male <- gtkLabel("b")
 lbl_b_female <- gtkLabel("b")
 gtkLabelSetWidthChars(lbl_b_male, 5) 
 gtkLabelSetWidthChars(lbl_b_female, 5) 
j=0   
tbl_AB$Attach(gtkLabel("b"),i, i+1,  j, j+1)  
j=j+1
tbl_AB$Attach(gtkLabel("b"),i, i+1,  j, j+1)

i=i+1
 # column 5 
entryAB_B_M <- gtkEntry()
gtkEntrySetWidthChars(entryAB_B_M, NUMERICAL_ENTRY_LENGTH)  
entryAB_B_F <- gtkEntry() 
gtkEntrySetWidthChars(entryAB_B_F, NUMERICAL_ENTRY_LENGTH)

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## additional code for BEMTOOL integration
if (IN_BEMTOOL) {
gtkEntrySetText(entryAB_B_M, Populations[[ALADYM_spe]]@lengthweight$b[1] )
gtkEntrySetText(entryAB_B_F, Populations[[ALADYM_spe]]@lengthweight$b[2] )
gtkEntrySetEditable(entryAB_B_M, FALSE)
gtkEntrySetEditable(entryAB_B_F, FALSE)
}
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------

j=0    
tbl_AB$Attach(entryAB_B_M,i, i+1, j, j+1)  
j=j+1
tbl_AB$Attach(entryAB_B_F,i, i+1,  j, j+1)
