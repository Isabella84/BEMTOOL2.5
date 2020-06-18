# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# initializating  objects

#GSAs <<- read.csv(paste(getwd(), "/bmtgui/utils/GSAs.bmt", sep="") , sep=";", header=F)
SPECIESs <<- read.csv(paste(getwd(), "/bmtgui/utils/species.bmt", sep="") , sep=";", header=F)
GEARs <<- read.csv(paste(getwd(), "/bmtgui/utils/Gear.bmt", sep="") , sep=";", header=F)
FLEETs <<- read.csv(paste(getwd(), "/bmtgui/utils/FleetSegment.bmt", sep="") , sep=";", header=F)



#gsas <<- data.frame(matrix(nrow=nrow(GSAs),ncol=1))
#gsas[,1] <- GSAs[,1]
#colnames(gsas) <- "GSA"


loas <<- data.frame(matrix(nrow=nrow(FLEETs),ncol=1))
loas[,1] <- FLEETs[,1]
colnames(loas) <- "Fleet"

speciess <<- data.frame(matrix(nrow=nrow(SPECIESs),ncol=1))
speciess[,1] <- SPECIESs[,1]
colnames(speciess) <- "Species"

gears <<- data.frame(matrix(nrow=nrow(GEARs),ncol=1))
gears[,1] <- GEARs[,1]
colnames(gears) <- "Fishing Techniques"

nostockss <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostockss) <- "Stocks"

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <- "Fleet segment"

# #################################################################################################################



 tbl_CS <- gtkTable(3,4,homogeneous = FALSE)
 tbl_CS$SetRowSpacings(7)
 tbl_CS$SetColSpacings(10)
 tbl_CS$SetBorderWidth(5)


  i = 0   # column 1 
  j=0 
tbl_CS$Attach(gtkLabel("Choose Species"),i, i+1, j, j+1)
 j=j+1  

  species.sw <<- gtkScrolledWindowNew(NULL, NULL)
species.sw$setShadowType("etched-in")
species.sw$setPolicy("automatic", "automatic")
species.sw$SetUsize(120, dim_big_tables)  
species_list <<- list()
speciesIndex <<- 0
# ------------------------------
# create model
species.create_model()
# create tree view
species.treeview <<- gtkTreeViewNewWithModel(species.model)
species.treeview$setRulesHint(TRUE)
species.treeview$getSelection()$setMode("single")
species.add_columns(species.treeview)
species.sw$add(species.treeview) 
   
tbl_CS$Attach(species.sw,i, i+1,  j, j+1)  

    
   i = i+1  # column 2
  j=0
tbl_CS$Attach(gtkLabel("Choose Fishing Tecnique"),i, i+1, j, j+1)
 j=j+1


   gear.sw <<- gtkScrolledWindowNew(NULL, NULL)
gear.sw$setShadowType("etched-in")
gear.sw$setPolicy("automatic", "automatic")
gear.sw$SetUsize(120, dim_big_tables)  
gear_list <<- list()
gearIndex <<- 0
# ------------------------------
# create model
gear.create_model()
# create tree view
gear.treeview <<- gtkTreeViewNewWithModel(gear.model)
gear.treeview$setRulesHint(TRUE)
gear.treeview$getSelection()$setMode("single")
gear.add_columns(gear.treeview)
gear.sw$add(gear.treeview) 
   
tbl_CS$Attach(gear.sw,i, i+1,  j, j+1) 


i = i+1  # column 3
  j=0
tbl_CS$Attach(gtkLabel("Choose LOA"),i, i+1, j, j+1)
j=j+1


loa.sw <<- gtkScrolledWindowNew(NULL, NULL)
loa.sw$setShadowType("etched-in")
loa.sw$setPolicy("automatic", "automatic")
loa.sw$SetUsize(120, dim_big_tables)  
loa_list <<- list()
loaIndex <<- 0
# ------------------------------
# create model
loa.create_model()
# create tree view
loa.treeview <<- gtkTreeViewNewWithModel(loa.model)
loa.treeview$setRulesHint(TRUE)
loa.treeview$getSelection()$setMode("GTK_SELECTION_MULTIPLE")
loa.add_columns(loa.treeview)
loa.sw$add(loa.treeview)    
tbl_CS$Attach(loa.sw,i, i+1,  j, j+1)  


   i = i+1  # column 4
  j=0
  tbl_CS$Attach(gtkLabel("Selected Stock(s) and Fleet Segment(s)"),i, i+1, j, j+1)
 j=j+1

#btn_add_association <- gtkButton()
#gtkButtonSetLabel(btn_add_association, "      Add to list      ")
#btn_add_association$AddCallback("clicked", addInteractions)
#tbl_CS$Attach(btn_add_association,i, i+1,  j, j+1)   





interaction.sw <<- gtkScrolledWindowNew(NULL, NULL)
interaction.sw$setShadowType("etched-in")
interaction.sw$setPolicy("automatic", "automatic")
interaction.sw$SetUsize(200, dim_big_tables)  
interaction_list <<- list()
interactionIndex <<- 0
# ------------------------------
# create model
interaction.create_model()
# create tree view
interaction.treeview <<- gtkTreeViewNewWithModel(interaction.model)
interaction.treeview$setRulesHint(TRUE)
interaction.treeview$getSelection()$setMode("GTK_SELECTION_MULTIPLE")
interaction.add_columns(interaction.treeview)
interaction.sw$add(interaction.treeview)    
tbl_CS$Attach(interaction.sw,i, i+2,  j, j+1)  

 j=j+1
btn_add_association <- gtkButton()
gtkButtonSetLabel(btn_add_association, "      Add     ")
btn_add_association$AddCallback("clicked", addInteractions)
tbl_CS$Attach(btn_add_association,i, i+1,  j, j+1)    
  tbl_CS$Attach(btn_add_association,i, i+1, j, j+1)
  
btn_remove_interaction <- gtkButton()
gtkButtonSetLabel(btn_remove_interaction, "      Remove      ")
btn_remove_interaction$AddCallback("clicked", removeInteractions)
tbl_CS$Attach(btn_remove_interaction,i+1, i+2, j, j+1)


hboxCaseStudy <- gtkHBox(homogeneous = FALSE, 5)
hboxCaseStudy$packStart(tbl_CS, expand = TRUE, fill = TRUE, padding = 10)
#hboxCaseStudy$packStart(interaction.sw, expand = FALSE, fill = TRUE, padding = 10)

# vbox_casestudy$packStart(hboxCaseStudy_general, expand = FALSE, fill = TRUE, padding = 10)      





entry_CaseStudy_name <- gtkEntry()
gtkEntrySetWidthChars(entry_CaseStudy_name, 15)

btn_browse_casestudypath <<- gtkButton()
gtkButtonSetLabel(btn_browse_casestudypath, "Browse...")
btn_browse_casestudypath$AddCallback("clicked", assign_casestudypath)

entry_casestudy_StartYear_simulation <- gtkEntry()
gtkEntrySetWidthChars(entry_casestudy_StartYear_simulation, NUMERICAL_ENTRY_LENGTH) 

entry_casestudy_EndYear_simulation <- gtkEntry()
gtkEntrySetWidthChars(entry_casestudy_EndYear_simulation, NUMERICAL_ENTRY_LENGTH)    

entry_casestudy_EndYear_forecast <- gtkEntry()
gtkEntrySetWidthChars(entry_casestudy_EndYear_forecast, NUMERICAL_ENTRY_LENGTH)   

entry_casestudy_NOSTOCK <- gtkEntry()
gtkEntrySetWidthChars(entry_casestudy_NOSTOCK, NUMERICAL_ENTRY_LENGTH) 

entry_casestudy_NOFLEETSEGMENT <- gtkEntry()
gtkEntrySetWidthChars(entry_casestudy_NOFLEETSEGMENT, NUMERICAL_ENTRY_LENGTH) 


 lbl_casestudy <-  gtkEntry()
       gtkEntrySetWidthChars(lbl_casestudy, 70) 
         gtkEntrySetEditable(lbl_casestudy,F)

   tbl_CS_name <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_CS_name$SetRowSpacings(7)
 tbl_CS_name$SetColSpacings(10)
 tbl_CS_name$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_CS_name$Attach(gtkLabel("Insert the Case study name    "),i, i+1, j, j+1)       
i=i+1 # column 1
 j=0
   tbl_CS_name$Attach(entry_CaseStudy_name,i, i+1, j, j+1) 





 tbl_CS_generalsettings <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_CS_generalsettings$SetRowSpacings(7)
 tbl_CS_generalsettings$SetColSpacings(10)
 tbl_CS_generalsettings$SetBorderWidth(5)
 
 i= 0
 j=0
  tbl_CS_generalsettings$Attach(gtkLabel("Start year of simulation"),i, i+1, j, j+1)
 
i=i+1 # column 3
 j=0
tbl_CS_generalsettings$Attach(entry_casestudy_StartYear_simulation,i, i+1,  j, j+1) 

   i=i+1 # column 4
 j=0

  tbl_CS_generalsettings$Attach(gtkLabel("End year of simulation"),i, i+1, j, j+1)
    j=j+1
  tbl_CS_generalsettings$Attach(gtkLabel("End year of forecast"),i, i+1, j, j+1) 

  
    i=i+1 # column 5
 j=0
  tbl_CS_generalsettings$Attach(entry_casestudy_EndYear_simulation,i, i+1, j, j+1)
     j=j+1
  tbl_CS_generalsettings$Attach(entry_casestudy_EndYear_forecast,i, i+1, j, j+1) 


 tbl_CS_next <- gtkVBox(homogeneous = FALSE, 5)

#  tbl_CS_next <- gtkTable(rows = 2, columns = 2, homogeneous = FALSE)
# tbl_CS_next$SetRowSpacings(7)
# tbl_CS_next$SetColSpacings(10)
# tbl_CS_next$SetBorderWidth(5)
 
 i= 0
 j=0
  hboxCaseStudyNOSTOCK <- gtkHBox(homogeneous = FALSE, 5)
hboxCaseStudyNOSTOCK$packStart(gtkLabel("N. of stocks"), expand = FALSE, fill = F, padding = 10)
hboxCaseStudyNOSTOCK$packStart(entry_casestudy_NOSTOCK, expand = FALSE, fill = F, padding = 10)

tbl_CS_next$packStart(hboxCaseStudyNOSTOCK, expand = FALSE, fill = TRUE, padding = 10)

 # tbl_CS_next$Attach(hboxCaseStudyNOSTOCK,i, i+1, j, j+1) 
  
gtkEntrySetText(entry_casestudy_NOSTOCK, 0 )
  gtkEntrySetEditable(entry_casestudy_NOSTOCK, FALSE) 

 j=j+1 

nostocks.sw <<- gtkScrolledWindowNew(NULL, NULL)
nostocks.sw$setShadowType("etched-in")
nostocks.sw$setPolicy("automatic", "automatic")
nostocks.sw$SetUsize(200, 200)  
nostocks_list <<- list()
nostocksIndex <<- 0
# ------------------------------
# create model
nostocks.create_model()
# create tree view
nostocks.treeview <<- gtkTreeViewNewWithModel(nostocks.model)
nostocks.treeview$setRulesHint(TRUE)
nostocks.treeview$getSelection()$setMode("none")
nostocks.add_columns(nostocks.treeview)
nostocks.sw$add(nostocks.treeview)    

tbl_CS_next$packStart(nostocks.sw, expand = FALSE, fill = TRUE, padding = 10)

#tbl_CS_next$Attach(nostocks.sw,i, i+2,  j, j+1)  
  
 j=j+1
 
   hboxCaseStudyNOFLEETSEGMENT <- gtkHBox(homogeneous = FALSE, 5)
hboxCaseStudyNOFLEETSEGMENT$packStart(gtkLabel("N. of fleet segments"), expand = FALSE, fill = F, padding = 10)
hboxCaseStudyNOFLEETSEGMENT$packStart(entry_casestudy_NOFLEETSEGMENT, expand = FALSE, fill = F, padding = 10)

tbl_CS_next$packStart(hboxCaseStudyNOFLEETSEGMENT, expand = FALSE, fill = TRUE, padding = 10)

#  tbl_CS_next$Attach(hboxCaseStudyNOFLEETSEGMENT,i, i+1, j, j+1)
  
  gtkEntrySetText(entry_casestudy_NOFLEETSEGMENT, 0)
  gtkEntrySetEditable(entry_casestudy_NOFLEETSEGMENT, FALSE) 
  
  j=j+1

nofleetsegments.sw <<- gtkScrolledWindowNew(NULL, NULL)
nofleetsegments.sw$setShadowType("etched-in")
nofleetsegments.sw$setPolicy("automatic", "automatic")
nofleetsegments.sw$SetUsize(200, 200)  
nofleetsegments_list <<- list()
nofleetsegmentsIndex <<- 0
# ------------------------------
# create model
nofleetsegments.create_model()
# create tree view
nofleetsegments.treeview <<- gtkTreeViewNewWithModel(nofleetsegments.model)
nofleetsegments.treeview$setRulesHint(TRUE)
nofleetsegments.treeview$getSelection()$setMode("single")
nofleetsegments.add_columns(nofleetsegments.treeview)
nofleetsegments.sw$add(nofleetsegments.treeview)    

tbl_CS_next$packStart(nofleetsegments.sw, expand = FALSE, fill = TRUE, padding = 10)

#tbl_CS_next$Attach(nofleetsegments.sw,i, i+2,  j, j+1)  
  

 tbl_CS_path <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_CS_path$SetRowSpacings(7)
 tbl_CS_path$SetColSpacings(10)
 tbl_CS_path$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_CS_path$Attach(gtkLabel("Select the Case study folder    "),i, i+1, j, j+1)       
i=i+1 # column 1
 j=0
   tbl_CS_path$Attach(btn_browse_casestudypath,i, i+1, j, j+1) 
 i=i+1 # column 2
 j=0
   tbl_CS_path$Attach(lbl_casestudy,i, i+1, j, j+1) 




frame_casestudy_general <- gtkFrame(" General settings ")   
frame_casestudy_interaction <- gtkFrame(" Stock-Fleet interactions ")   


hbox_casestudy_generalsetting <- gtkHBox(homogeneous = FALSE, 5)        # includes label and text area
hbox_casestudy_path <- gtkHBox(homogeneous = FALSE, 5)                   # includes label and text area
hbox_casestudy_name <- gtkHBox(homogeneous = FALSE, 5)                     # includes label and text area
hbox_two <- gtkHBox(homogeneous = FALSE, 5)                
hbox_casestudy <- gtkHBox(homogeneous = FALSE, 5)            

vbox_casestudy <- gtkVBox(FALSE, 5)                              # includes all sw of gsa, loa, species, etc...
vbox_casestudy_path <- gtkVBox(FALSE, 5)                        # includes the 3 hBOX above

vbox_casestudy_interaction <- gtkVBox(FALSE, 5)                 # includes hboxCaseStudy

vbox_CS <- gtkVBox(homogeneous = FALSE, 5) 

hbox_casestudy_generalsetting$packStart(tbl_CS_generalsettings, expand = FALSE, fill = FALSE, padding = 10) 
hbox_casestudy_path$packStart(tbl_CS_path, expand = FALSE, fill = FALSE, padding = 10) 
hbox_casestudy_name$packStart(tbl_CS_name, expand = FALSE, fill = FALSE, padding = 10) 

vbox_casestudy_path$packStart(hbox_casestudy_name, expand = FALSE, fill = FALSE, padding = 5)  
vbox_casestudy_path$packStart(hbox_casestudy_generalsetting, expand = FALSE, fill = FALSE, padding = 5)
vbox_casestudy_path$packStart(hbox_casestudy_path, expand = FALSE, fill = FALSE, padding = 5)

vbox_casestudy_interaction$packStart(hboxCaseStudy, expand = TRUE, fill = TRUE, padding = 10) 

#hbox_two <- gtkHBox(homogeneous = FALSE, 5)
#hbox_two$packStart(vbox_casestudy_path, expand = TRUE, fill = TRUE, padding = 10)     
#hbox_two$packStart(tbl_CS_next, expand = TRUE, fill = TRUE, padding = 10)           

frame_casestudy_general$add(vbox_casestudy_path) 
frame_casestudy_interaction$add(vbox_casestudy_interaction) 


vbox_casestudy$packStart(frame_casestudy_general, expand = FALSE, fill = FALSE, padding = 10) 
vbox_casestudy$packStart(frame_casestudy_interaction, expand = FALSE, fill = TRUE, padding = 10)    #true


hbox_casestudy$packStart(vbox_casestudy, expand = FALSE, fill = FALSE, padding = 10) 
hbox_casestudy$packStart(tbl_CS_next, expand = T, fill = T, padding = 10) 

vbox_CS$packStart(hbox_casestudy, expand = FALSE, fill = TRUE, padding = 15)         #true

#vbox_CS$packStart(, expand = FALSE, fill = TRUE, padding = 10) 

#  vbox_casestudy$packStart(interaction.sw, expand = FALSE, fill = TRUE, padding = 10)     