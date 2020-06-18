# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 
  entry_assessment_numberAgeClasses_XSA <- gtkEntry()
  gtkEntrySetWidthChars(entry_assessment_numberAgeClasses_XSA, NUMERICAL_ENTRY_LENGTH) 
  
  entry_assessment_XSA_results <-  gtkEntry()
   gtkEntrySetWidthChars(entry_assessment_XSA_results, 100)
   gtkEntrySetEditable(entry_assessment_XSA_results,F)
     
    entry_assessment_assess_Catchfleet <-  gtkEntry() 
     gtkEntrySetWidthChars(entry_assessment_assess_Catchfleet, 100) 
      gtkEntrySetEditable(entry_assessment_assess_Catchfleet,F)
      
      entry_assessment_assess_Ffleet <-  gtkEntry()
       gtkEntrySetWidthChars(entry_assessment_assess_Ffleet, 100) 
        gtkEntrySetEditable(entry_assessment_assess_Ffleet,F)
        
        entry_assessment_assess_RPs <-  gtkEntry()  
         gtkEntrySetWidthChars(entry_assessment_assess_RPs, 100) 
          gtkEntrySetEditable(entry_assessment_assess_RPs,F)   

  btn_browse_XSAresults_path <- gtkButton()
gtkButtonSetLabel(btn_browse_XSAresults_path, "Browse...")
btn_browse_XSAresults_path$AddCallback("clicked", assign_bio_XSAresults_path)
  btn_browse_assess_Catchfleet_path <- gtkButton()
gtkButtonSetLabel(btn_browse_assess_Catchfleet_path, "Browse...")
btn_browse_assess_Catchfleet_path$AddCallback("clicked", assign_bio_assess_Catchfleet_path)
  btn_browse_assess_Ffleet_path <- gtkButton()
gtkButtonSetLabel(btn_browse_assess_Ffleet_path, "Browse...")
btn_browse_assess_Ffleet_path$AddCallback("clicked", assign_bio_assess_Ffleet_path)
  btn_browse_assess_RPs_path <- gtkButton()
gtkButtonSetLabel(btn_browse_assess_RPs_path, "Browse...")
btn_browse_assess_RPs_path$AddCallback("clicked", assign_bio_assess_RPs_path)

   

 tbl_assessment_XSA <- gtkTable(4,3,homogeneous = FALSE)
 tbl_assessment_XSA$SetRowSpacings(7)
 tbl_assessment_XSA$SetColSpacings(10)
 tbl_assessment_XSA$SetBorderWidth(5)
 


  i = 0   # column 1 
  j=0 
tbl_assessment_XSA$Attach(gtkLabel("Number of age classes (combined sex)"),i, i+1, j, j+1)
  
   i = i+1  # column 2
  j=0
tbl_assessment_XSA$Attach(entry_assessment_numberAgeClasses_XSA,i, i+1, j, j+1)




 tbl_assessment_XSA_paths <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_assessment_XSA_paths$SetRowSpacings(7)
 tbl_assessment_XSA_paths$SetColSpacings(10)
 tbl_assessment_XSA_paths$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_assessment_XSA_paths$Attach(gtkLabel("XSA results (.dat file)    "),i, i+1, j, j+1)     
   j=j+1
    tbl_assessment_XSA_paths$Attach(gtkLabel("Catch by fleet (.csv file)    "),i, i+1, j, j+1)   
       j=j+1
    tbl_assessment_XSA_paths$Attach(gtkLabel("F by fleet (.csv file)    "),i, i+1, j, j+1)   
       j=j+1
    tbl_assessment_XSA_paths$Attach(gtkLabel("Reference points (.csv file)    "),i, i+1, j, j+1)   

i=i+1 # column 1  
 j=0
   tbl_assessment_XSA_paths$Attach(btn_browse_XSAresults_path,i, i+1, j, j+1) 
   j=j+1
   tbl_assessment_XSA_paths$Attach(btn_browse_assess_Catchfleet_path,i, i+1, j, j+1) 
   j=j+1
   tbl_assessment_XSA_paths$Attach(btn_browse_assess_Ffleet_path,i, i+1, j, j+1) 
   j=j+1
   tbl_assessment_XSA_paths$Attach(btn_browse_assess_RPs_path,i, i+1, j, j+1) 


 i=i+1 # column 2
 j=0
   tbl_assessment_XSA_paths$Attach(entry_assessment_XSA_results,i, i+1, j, j+1) 
    j=j+1
   tbl_assessment_XSA_paths$Attach(entry_assessment_assess_Catchfleet,i, i+1, j, j+1) 
      j=j+1
   tbl_assessment_XSA_paths$Attach(entry_assessment_assess_Ffleet,i, i+1, j, j+1) 
       j=j+1
   tbl_assessment_XSA_paths$Attach(entry_assessment_assess_RPs,i, i+1, j, j+1) 


hbox_assessment_XSA <- gtkHBox(homogeneous = FALSE, 5)
hbox_assessment_XSA_paths <- gtkHBox(homogeneous = FALSE, 5)                  

vbox_assessment_XSA <- gtkVBox(FALSE, 5)     
vbox_assessment_XSA_outer <- gtkVBox(FALSE, 5)                  

hbox_assessment_XSA$packStart(tbl_assessment_XSA, expand = FALSE, fill = FALSE, padding = 10) 
hbox_assessment_XSA_paths$packStart(tbl_assessment_XSA_paths, expand = FALSE, fill = FALSE, padding = 10) 

vbox_assessment_XSA$packStart(hbox_assessment_XSA, expand = F, fill = F, padding = 0)
 vbox_assessment_XSA$packStart(hbox_assessment_XSA_paths, expand = F, fill = F, padding = 0)

  


#hbox_two <- gtkHBox(homogeneous = FALSE, 5)
#hbox_two$packStart(vbox_casestudy_path, expand = TRUE, fill = TRUE, padding = 10)     
#hbox_two$packStart(tbl_CS_next, expand = TRUE, fill = TRUE, padding = 10)           

#frame_assessment_XSA$add(vbox_assessment_XSA) 

vbox_assessment_XSA_outer$packStart(vbox_assessment_XSA, expand = F, fill = F, padding = 10) 
