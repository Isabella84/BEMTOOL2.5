# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




bmt_entryAB_A_M <- gtkEntry()   
bmt_entryAB_A_F <- gtkEntry()
bmt_entryAB_B_M <- gtkEntry()
bmt_entryAB_B_F <- gtkEntry() 

bmt_entryVBF_linf_M <- gtkEntry()
bmt_entryVBF_linf_F <- gtkEntry()  
bmt_entryVBF_t0_M <- gtkEntry()
bmt_entryVBF_t0_F <- gtkEntry()
bmt_entryVBF_k_M <- gtkEntry()
bmt_entryVBF_k_F <- gtkEntry()

bmt_entryOGIVEL50_M <- gtkEntry()
bmt_entryOGIVEL50_F <- gtkEntry()

bmt_entryMR_M <- gtkEntry()
bmt_entryMR_F <- gtkEntry()

bmt_entryLS_M <- gtkEntry() 
bmt_entryLS_F <- gtkEntry() 

bmt_entry_sexratio <- gtkEntry() 

gtkEntrySetWidthChars(bmt_entryAB_A_M, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(bmt_entryAB_A_F, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(bmt_entryAB_B_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryAB_B_F, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetWidthChars(bmt_entryVBF_linf_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryVBF_linf_F, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(bmt_entryVBF_t0_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryVBF_t0_F, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(bmt_entryVBF_k_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryVBF_k_F, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetWidthChars(bmt_entryOGIVEL50_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryOGIVEL50_F, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetWidthChars(bmt_entryMR_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryMR_F, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetWidthChars(bmt_entryLS_M, NUMERICAL_ENTRY_LENGTH)  
gtkEntrySetWidthChars(bmt_entryLS_F, NUMERICAL_ENTRY_LENGTH)

gtkEntrySetWidthChars(bmt_entry_sexratio, NUMERICAL_ENTRY_LENGTH)


bmt_tbl_AB <- gtkTable(10,4,homogeneous = FALSE)
bmt_tbl_AB$SetRowSpacings(5)
bmt_tbl_AB$SetColSpacings(30)
bmt_tbl_AB$SetBorderWidth(5)

i=0  # column 0 
j=0 
j=j+1
bmt_tbl_AB$Attach(gtkLabel("LIFE SPAN"),i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(gtkLabel("GROWTH FUNCTION"),i, i+1, j, j+1) 
j=j+1
j=j+1
j=j+1
bmt_tbl_AB$Attach(gtkLabel("MATURITY"),i, i+1, j, j+1) 
j=j+1
j=j+1
bmt_tbl_AB$Attach(gtkLabel("L-W RELATIONSHIP"),i, i+1, j, j+1) 
j=j+1
j=j+1
bmt_tbl_AB$Attach(gtkLabel("SEX RATIO"),i, i+1, j, j+1) 

i=i+1  # column 1  
j=0
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("Years"),i, i+1, j, j+1) 
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("t0 [years]"),i, i+1, j, j+1) 
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("K [years^-1]"),i, i+1, j, j+1) 
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("Linfinity [mm]"),i, i+1, j, j+1)  
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1) 
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)       
j=j+1 
bmt_tbl_AB$Attach(gtkLabel("a [g/mm^b]"),i, i+1, j, j+1)  
j=j+1  
bmt_tbl_AB$Attach(gtkLabel("b"),i, i+1, j, j+1) 
j=j+1  
bmt_tbl_AB$Attach(bmt_entry_sexratio,i, i+1, j, j+1) 


i=i+1  # column 2 
j=0 
bmt_tbl_AB$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
j=j+1
bmt_tbl_AB$Attach(bmt_entryLS_M,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_t0_M,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_k_M,i, i+1, j, j+1)
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_linf_M,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryOGIVEL50_M,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryMR_M,i, i+1, j, j+1)    
j=j+1 
bmt_tbl_AB$Attach(bmt_entryAB_A_M,i, i+1, j, j+1)
j=j+1  
bmt_tbl_AB$Attach(bmt_entryAB_B_M,i, i+1, j, j+1)

i=i+1  # column 3
j=0 
bmt_tbl_AB$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
j=j+1
bmt_tbl_AB$Attach(bmt_entryLS_F,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_t0_F,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_k_F,i, i+1, j, j+1)  
j=j+1
bmt_tbl_AB$Attach(bmt_entryVBF_linf_F,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryOGIVEL50_F,i, i+1, j, j+1) 
j=j+1
bmt_tbl_AB$Attach(bmt_entryMR_F,i, i+1, j, j+1)         
j=j+1
bmt_tbl_AB$Attach(bmt_entryAB_A_F,i, i+1, j, j+1)
j=j+1  
bmt_tbl_AB$Attach(bmt_entryAB_B_F,i, i+1, j, j+1)




vbox_biosettings$packStart(bmt_tbl_AB, expand = T, fill = TRUE, padding = 0)         #true
