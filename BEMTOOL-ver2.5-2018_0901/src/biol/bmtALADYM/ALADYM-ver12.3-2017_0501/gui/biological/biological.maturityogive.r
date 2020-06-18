# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






 tbl_OGIVE <- gtkTable(4,5,homogeneous = FALSE)
 tbl_OGIVE$SetRowSpacings(7)
 tbl_OGIVE$SetColSpacings(10)
 tbl_OGIVE$SetBorderWidth(5)

 i=0 # column 0
 j=0
 tbl_OGIVE$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
 j=j+1
 j=j+1 
  tbl_OGIVE$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)   
  
  i = i+1   # column 1 
  j=0 
tbl_OGIVE$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1)
 j=j+1  
tbl_OGIVE$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1)
 j=j+1  
tbl_OGIVE$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)
    
   i = i+1  # column 2
  j=0
tbl_OGIVE$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("distribution"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("distribution"),i, i+1, j, j+1)

   i = i+1  # column 3

combo_L50dis_M <- gtkComboBoxNewText()
gSignalConnect(combo_L50dis_M, "changed", change_maturityogive_L50_M)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_L50dis_M$appendText(choice) 
}
}

combo_L75L25dis_M <- gtkComboBoxNewText()
gSignalConnect(combo_L75L25dis_M, "changed", change_maturityogive_L75L25_M)
for (choice in DISTRIBUTION) {
if (choice != "Gamma")   {
 combo_L75L25dis_M$appendText(choice)  
 }
 }

 
combo_L50dis_F <- gtkComboBoxNewText()
gSignalConnect(combo_L50dis_F, "changed", change_maturityogive_L50_F)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_L50dis_F$appendText(choice) 
}
 }

combo_L75L25dis_F <- gtkComboBoxNewText()
gSignalConnect(combo_L75L25dis_F, "changed", change_maturityogive_L75L25_F)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
 combo_L75L25dis_F$appendText(choice)
 }
   }

 
 j=0
 tbl_OGIVE$Attach(combo_L50dis_M,i, i+1, j, j+1)
  j=j+1
tbl_OGIVE$Attach(combo_L75L25dis_M,i, i+1, j, j+1)
   j=j+1
tbl_OGIVE$Attach(combo_L50dis_F,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(combo_L75L25dis_F,i, i+1,j, j+1)

i = i+1   # column 4
  j=0
tbl_OGIVE$Attach(gtkLabel("min"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("min"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("min"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("min"),i, i+1,j, j+1)

    i = i+1   # column 5

entryOGIVEL50_M_min <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_M_min, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_M_min <- gtkEntry()      
gtkEntrySetWidthChars(entryOGIVEL75L25_M_min, NUMERICAL_ENTRY_LENGTH)  

entryOGIVEL50_F_min <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_F_min, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_F_min <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_F_min, NUMERICAL_ENTRY_LENGTH) 


j=0
tbl_OGIVE$Attach(entryOGIVEL50_M_min,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_M_min,i, i+1, j, j+1)
 j=j+1 
tbl_OGIVE$Attach(entryOGIVEL50_F_min,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_F_min,i, i+1, j, j+1)
   
    i = i+1   # column 6
j=0
tbl_OGIVE$Attach(gtkLabel("max"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("max"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("max"),i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(gtkLabel("max"),i, i+1, j, j+1)
    
     i = i+1   # column 7

entryOGIVEL50_M_max <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_M_max, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_M_max <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_M_max, NUMERICAL_ENTRY_LENGTH) 
 
entryOGIVEL50_F_max <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_F_max, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_F_max <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_F_max, NUMERICAL_ENTRY_LENGTH) 

j=0
tbl_OGIVE$Attach(entryOGIVEL50_M_max,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_M_max,i, i+1, j, j+1)
 j=j+1 
tbl_OGIVE$Attach(entryOGIVEL50_F_max,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_F_max,i, i+1, j, j+1)

     i = i+1   # column 8
lbl_A_maturityogive_L50_M <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_maturityogive_L50_M, LABEL_LENGTH)  
lbl_A_maturityogive_L75L25_M <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_maturityogive_L75L25_M, LABEL_LENGTH)   
lbl_A_maturityogive_L50_F <- gtkLabel("A") 
gtkLabelSetWidthChars(lbl_A_maturityogive_L50_F, LABEL_LENGTH)  
lbl_A_maturityogive_L75L25_F <- gtkLabel("A")  
gtkLabelSetWidthChars(lbl_A_maturityogive_L75L25_F, LABEL_LENGTH)  

j=0
tbl_OGIVE$Attach(lbl_A_maturityogive_L50_M,i, i+1,j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_A_maturityogive_L75L25_M,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_A_maturityogive_L50_F,i, i+1,j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_A_maturityogive_L75L25_F,i, i+1, j, j+1)

    i = i+1    # column 9

entryOGIVEL50_M_a <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_M_a, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_M_a <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_M_a, NUMERICAL_ENTRY_LENGTH) 

entryOGIVEL50_F_a <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_F_a, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_F_a <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_F_a, NUMERICAL_ENTRY_LENGTH)  

j=0
tbl_OGIVE$Attach(entryOGIVEL50_M_a,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_M_a,i, i+1, j, j+1)
 j=j+1 
tbl_OGIVE$Attach(entryOGIVEL50_F_a,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_F_a,i, i+1, j, j+1)

     i = i+1   # column 10
lbl_B_maturityogive_L50_M <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_maturityogive_L50_M, LABEL_LENGTH)  
lbl_B_maturityogive_L75L25_M <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_maturityogive_L50_M, LABEL_LENGTH)   
lbl_B_maturityogive_L50_F <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_maturityogive_L50_M, LABEL_LENGTH)   
lbl_B_maturityogive_L75L25_F <- gtkLabel("B") 
gtkLabelSetWidthChars(lbl_B_maturityogive_L50_M, LABEL_LENGTH)   

j=0
tbl_OGIVE$Attach(lbl_B_maturityogive_L50_M,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_B_maturityogive_L75L25_M,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_B_maturityogive_L50_F,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(lbl_B_maturityogive_L75L25_F,i, i+1, j, j+1)
    
     i = i+1   # column 11

entryOGIVEL50_M_b <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_M_b, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_M_b <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_M_b, NUMERICAL_ENTRY_LENGTH)    

entryOGIVEL50_F_b <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL50_F_b, NUMERICAL_ENTRY_LENGTH)  
entryOGIVEL75L25_F_b <- gtkEntry()
gtkEntrySetWidthChars(entryOGIVEL75L25_F_b, NUMERICAL_ENTRY_LENGTH)

j=0
tbl_OGIVE$Attach(entryOGIVEL50_M_b,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_M_b,i, i+1, j, j+1)
 j=j+1  
tbl_OGIVE$Attach(entryOGIVEL50_F_b,i, i+1, j, j+1)
 j=j+1
tbl_OGIVE$Attach(entryOGIVEL75L25_F_b,i, i+1, j, j+1)

gtkComboBoxSetActive(combo_L50dis_F, 0 )
gtkComboBoxSetActive(combo_L75L25dis_F, 0 ) 
gtkComboBoxSetActive(combo_L50dis_M, 0 )
gtkComboBoxSetActive(combo_L75L25dis_M, 0 )