# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 
  entry_assessment_numberAgeClasses_Report <- gtkEntry()
  gtkEntrySetWidthChars(entry_assessment_numberAgeClasses_Report, NUMERICAL_ENTRY_LENGTH) 
  
        entry_assessment_Reportresults <-  gtkEntry()
       gtkEntrySetWidthChars(entry_assessment_Reportresults, 70) 
       gtkEntrySetEditable(entry_assessment_Reportresults, F)
   

 tbl_assessment_Report <- gtkTable(4,11,homogeneous = FALSE)
 tbl_assessment_Report$SetRowSpacings(7)
 tbl_assessment_Report$SetColSpacings(10)
 tbl_assessment_Report$SetBorderWidth(5)
 


  i = 0   # column 1 
  j=0 
tbl_assessment_Report$Attach(gtkLabel("Number of age classes (combined sex)"),i, i+1, j, j+1)
  
   i = i+1  # column 2
  j=0
tbl_assessment_Report$Attach(entry_assessment_numberAgeClasses_Report,i, i+1, j, j+1)




 tbl_assessment_Report_paths <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_assessment_Report_paths$SetRowSpacings(7)
 tbl_assessment_Report_paths$SetColSpacings(10)
 tbl_assessment_Report_paths$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_assessment_Report_paths$Attach(gtkLabel("Select the External file    "),i, i+1, j, j+1)       
i=i+1 # column 1
 j=0
 #  tbl_assessment_Report_paths$Attach(btn_browse_XSAResults_path,i, i+1, j, j+1) 
 i=i+1 # column 2
 j=0
  # tbl_assessment_Report_paths$Attach(lbl_casestudy,i, i+1, j, j+1) 




hbox_assessment_Report <- gtkHBox(homogeneous = FALSE, 5)    
      

vbox_assessment_Report <- gtkVBox(FALSE, 5)     
vbox_assessment_Report_outer <- gtkVBox(FALSE, 5)                  

hbox_assessment_Report$packStart(tbl_assessment_Report, expand = FALSE, fill = FALSE, padding = 10) 


hbox_assessment_Report_file <- gtkHBox(homogeneous = FALSE, 5)
hbox_assessment_Report_file$packStart(gtkLabel("External file results (.csv file)"), expand = FALSE, fill = FALSE, padding = 10)     


btn_browse_Reportresults_path <- gtkButton()
gtkButtonSetLabel(btn_browse_Reportresults_path, "Browse...")
btn_browse_Reportresults_path$AddCallback("clicked", assign_bio_Reportresults_path)

hbox_assessment_Report_file$packStart(btn_browse_Reportresults_path, expand = FALSE, fill = FALSE, padding = 10) 


hbox_assessment_Report_file$packStart(entry_assessment_Reportresults, expand = FALSE, fill = FALSE, padding = 10) 

vbox_assessment_Report$packStart(hbox_assessment_Report, expand = F, fill = F, padding = 5)
 vbox_assessment_Report$packStart(hbox_assessment_Report_file, expand = F, fill = F, padding = 5)



vbox_assessment_Report_outer$packStart(vbox_assessment_Report, expand = T, fill = T, padding = 10) 
