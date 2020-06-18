# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 
  entry_assessment_numberAgeClasses_SURBA <- gtkEntry()
  gtkEntrySetWidthChars(entry_assessment_numberAgeClasses_SURBA, NUMERICAL_ENTRY_LENGTH) 
  
        entry_assessment_SURBAresults <-  gtkEntry()
       gtkEntrySetWidthChars(entry_assessment_SURBAresults, 70) 
       gtkEntrySetEditable(entry_assessment_SURBAresults, F)
   

 tbl_assessment_SURBA <- gtkTable(4,11,homogeneous = FALSE)
 tbl_assessment_SURBA$SetRowSpacings(7)
 tbl_assessment_SURBA$SetColSpacings(10)
 tbl_assessment_SURBA$SetBorderWidth(5)
 


  i = 0   # column 1 
  j=0 
tbl_assessment_SURBA$Attach(gtkLabel("Number of years for average"),i, i+1, j, j+1)
  
   i = i+1  # column 2
  j=0
tbl_assessment_SURBA$Attach(entry_assessment_numberAgeClasses_SURBA,i, i+1, j, j+1)




 tbl_assessment_SURBA_paths <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_assessment_SURBA_paths$SetRowSpacings(7)
 tbl_assessment_SURBA_paths$SetColSpacings(10)
 tbl_assessment_SURBA_paths$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_assessment_SURBA_paths$Attach(gtkLabel("Select the XSA Results file    "),i, i+1, j, j+1)       
i=i+1 # column 1
 j=0
 #  tbl_assessment_SURBA_paths$Attach(btn_browse_XSAResults_path,i, i+1, j, j+1) 
 i=i+1 # column 2
 j=0
  # tbl_assessment_SURBA_paths$Attach(lbl_casestudy,i, i+1, j, j+1) 




hbox_assessment_SURBA <- gtkHBox(homogeneous = FALSE, 5)    
      

vbox_assessment_SURBA <- gtkVBox(FALSE, 5)     
vbox_assessment_SURBA_outer <- gtkVBox(FALSE, 5)                  

hbox_assessment_SURBA$packStart(tbl_assessment_SURBA, expand = FALSE, fill = FALSE, padding = 10) 


hbox_assessment_SURBA_file <- gtkHBox(homogeneous = FALSE, 5)
hbox_assessment_SURBA_file$packStart(gtkLabel("SURBA results (.csv file)"), expand = FALSE, fill = FALSE, padding = 10)     


btn_browse_SURBAresults_path <- gtkButton()
gtkButtonSetLabel(btn_browse_SURBAresults_path, "Browse...")
btn_browse_SURBAresults_path$AddCallback("clicked", assign_bio_SURBAresults_path)

hbox_assessment_SURBA_file$packStart(btn_browse_SURBAresults_path, expand = FALSE, fill = FALSE, padding = 10) 


hbox_assessment_SURBA_file$packStart(entry_assessment_SURBAresults, expand = FALSE, fill = FALSE, padding = 10) 

vbox_assessment_SURBA$packStart(hbox_assessment_SURBA, expand = F, fill = F, padding = 5)
 vbox_assessment_SURBA$packStart(hbox_assessment_SURBA_file, expand = F, fill = F, padding = 5)



vbox_assessment_SURBA_outer$packStart(vbox_assessment_SURBA, expand = T, fill = T, padding = 10) 
