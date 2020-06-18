# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 vboxOFFSPRING <- gtkVBox(FALSE, 5)
			
 tbl_OFFSPRING <- gtkTable(1,12,homogeneous = FALSE)
 tbl_OFFSPRING$SetRowSpacings(15)
 tbl_OFFSPRING$SetColSpacings(10)
 tbl_OFFSPRING$SetBorderWidth(5)

 i=0   # column 0
 j=0
 tbl_OFFSPRING$Attach(gtkLabel("R [thousands]"),i, i+1, j, j+1) 

  i=i+1  # column 1
  j=0  
tbl_OFFSPRING$Attach(gtkLabel("error distribution"),i, i+1, j, j+1)

  i=i+1  # column 2
combo_OFFSPRING_rand <- gtkComboBoxNewText()
gSignalConnect(combo_OFFSPRING_rand, "changed", change_recruits_AB)
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_OFFSPRING_rand$appendText(choice) 
}
 }


j=0
tbl_OFFSPRING$Attach(combo_OFFSPRING_rand,i, i+1, j, j+1)

 i=i+1  # column 3 
 j=0
tbl_OFFSPRING$Attach(gtkLabel("min"),i, i+1, j, j+1)

 i=i+1   # column 4
entryOFFSPRING_rand_min <- gtkEntry()
gtkEntrySetWidthChars(entryOFFSPRING_rand_min, NUMERICAL_ENTRY_LENGTH)
 j=0
tbl_OFFSPRING$Attach(entryOFFSPRING_rand_min,i, i+1, j, j+1)

 i=i+1   # column 5
j=0
tbl_OFFSPRING$Attach(gtkLabel("max"),i, i+1, j, j+1)

 i=i+1   # column 6
entryOFFSPRING_rand_max <- gtkEntry()
gtkEntrySetWidthChars(entryOFFSPRING_rand_max, NUMERICAL_ENTRY_LENGTH)
j=0
tbl_OFFSPRING$Attach(entryOFFSPRING_rand_max,i, i+1, j, j+1)

 i=i+1   # column 7
j=0
lbl_A_recruits <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_recruits, LABEL_LENGTH) 

tbl_OFFSPRING$Attach(lbl_A_recruits,i, i+1, j, j+1)

 i=i+1    # column 8
entryOFFSPRING_rand_a <- gtkEntry()
gtkEntrySetWidthChars(entryOFFSPRING_rand_a, NUMERICAL_ENTRY_LENGTH)
 j=0
tbl_OFFSPRING$Attach(entryOFFSPRING_rand_a,i, i+1, j, j+1)


 i=i+1   # column 9
 j=0
 lbl_B_recruits <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_recruits, LABEL_LENGTH) 

tbl_OFFSPRING$Attach(lbl_B_recruits,i, i+1, j, j+1)

 i=i+1    # column 10
entryOFFSPRING_rand_b <- gtkEntry()
gtkEntrySetWidthChars(entryOFFSPRING_rand_b, NUMERICAL_ENTRY_LENGTH)
j=0
tbl_OFFSPRING$Attach(entryOFFSPRING_rand_b,i, i+1, j, j+1)

i=i+1  # column 11
  j=0  
tbl_OFFSPRING$Attach(gtkLabel("(100 runs)"),i, i+1, j, j+1)

vboxOFFSPRING$packStart(tbl_OFFSPRING, expand = TRUE, fill = TRUE, padding = 5)

# -------------------------------------
  
vboxOFFSPRING$packStart(gtkLabelNew("Monthly proportion of recruits"),FALSE, FALSE, 5)
monthlyOffsprings.sw <<- gtkScrolledWindowNew(NULL, NULL)
monthlyOffsprings.sw$setShadowType("etched-in")
monthlyOffsprings.sw$setPolicy("automatic", "automatic")
monthlyOffsprings.sw$SetUsize(100, 50)  
monthlyOffsprings <<- list()
monthlyOffspringsIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
monthlyOffsprings.create_model()
# create tree view
monthlyOffsprings.treeview <<- gtkTreeViewNewWithModel(monthlyOffsprings.model)
monthlyOffsprings.treeview$setRulesHint(TRUE)
monthlyOffsprings.treeview$getSelection()$setMode("single")
monthlyOffsprings.add_columns(monthlyOffsprings.treeview)
monthlyOffsprings.sw$add(monthlyOffsprings.treeview)      
#vboxOFFSPRING$packStart(monthlyOffsprings.sw , TRUE, TRUE, 0)

   


hboxOFFSPRING_tr <- gtkHBox(FALSE, 5)
hboxOFFSPRING_tr$packStart(gtkLabel("tr [months]"), expand = FALSE, fill = FALSE, padding = 5) 
entryOFFSPRING_tr <- gtkEntry()
gtkEntrySetText(entryOFFSPRING_tr, "0")
gtkEntrySetWidthChars(entryOFFSPRING_tr, 3)  
gSignalConnect(entryOFFSPRING_tr, "changed", change_tr)
hboxOFFSPRING_tr$packStart(entryOFFSPRING_tr, expand = FALSE, fill = FALSE, padding = 5)

hbox2_box_tr <- gtkHBox(FALSE, 5)
hbox2_box_tr$packStart(hboxOFFSPRING_tr, expand = FALSE, fill = FALSE, padding = 5)
hbox2_box_tr$packStart(monthlyOffsprings.sw, expand = T, fill = T, padding = 5)

vboxOFFSPRING$packStart(hbox2_box_tr, expand = FALSE, fill = FALSE, padding = 5)

gtkComboBoxSetActive(combo_OFFSPRING_rand, 0 )
