# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vbox_biosettings_chks <- gtkVBox(homogeneous = FALSE, 5)

hbox_biosettings_avg_years <- gtkHBox(homogeneous = FALSE, 5)
hbox_biosettings_externalRPs <- gtkHBox(homogeneous = FALSE, 5)

btn_browse_externalRPs <<- gtkButton()
gtkButtonSetLabel(btn_browse_externalRPs, "Browse...")
btn_browse_externalRPs$AddCallback("clicked", assign_bio_externalRPspath)

entry_biosettings_avg_years <- gtkEntry()
gtkEntrySetWidthChars(entry_biosettings_avg_years, NUMERICAL_ENTRY_LENGTH) 

 bmt_chk_Mcostant <- gtkCheckButton("Natural mortality (M) constant")
  bmt_chk_SRrelationship <- gtkCheckButton("Stock recruitment relationship")
  bmt_chk_runALADYM <- gtkCheckButton("Run ALADYM")
  gSignalConnect(bmt_chk_runALADYM, "toggled", deactivate_ALADYM_items)
   bmt_chk_RP_ALADYM_calc <- gtkCheckButton("Reference points (RPs) ALADYM calculation")
    bmt_chk_RP_ALADYM_use <- gtkCheckButton("Reference points ALADYM use")
gSignalConnect(bmt_chk_RP_ALADYM_use, "toggled", deactivate_RPs_externalfile)

 vbox_biosettings_chks$packStart(bmt_chk_Mcostant, expand = FALSE, fill = TRUE, padding = 3)         #true
  vbox_biosettings_chks$packStart(bmt_chk_SRrelationship, expand = FALSE, fill = TRUE, padding = 3)         #true  
   vbox_biosettings_chks$packStart(bmt_chk_runALADYM, expand = FALSE, fill = TRUE, padding = 3)         #true
    vbox_biosettings_chks$packStart(bmt_chk_RP_ALADYM_calc, expand = FALSE, fill = TRUE, padding = 3)         #true
     vbox_biosettings_chks$packStart(bmt_chk_RP_ALADYM_use, expand = FALSE, fill = TRUE, padding = 3)         #true

     lbl_biosettings_avg_years <- gtkLabel("Years for ALADYM RPs and forecast     ")
     hbox_biosettings_avg_years$packStart(lbl_biosettings_avg_years, expand = FALSE, fill = TRUE, padding = 10)         #true
     hbox_biosettings_avg_years$packStart(entry_biosettings_avg_years, expand = FALSE, fill = TRUE, padding = 10)         #true

     lbl_browse_externalRPs <-  gtkLabel("RPs external file ")
    # hbox_biosettings_externalRPs$packStart(lbl_browse_externalRPs, expand = FALSE, fill = TRUE, padding = 10)         #true
         hbox_biosettings_externalRPs$packStart(lbl_browse_externalRPs, expand = FALSE, fill = F, padding = 10)         #true
     hbox_biosettings_externalRPs$packStart(btn_browse_externalRPs, expand = FALSE, fill = F, padding = 10)         #true
       
       lbl_bio_externalRP <- gtkEntry()
         gtkEntrySetWidthChars(lbl_bio_externalRP, 37)  
                  gtkEntrySetEditable(lbl_bio_externalRP,F)
         hbox_biosettings_externalRPs$packStart(lbl_bio_externalRP, expand = FALSE, fill = TRUE, padding = 10)         #true


vbox_biosettings_simu$packStart(vbox_biosettings_chks, expand = FALSE, fill = TRUE, padding = 10)         #true
vbox_biosettings_simu$packStart(hbox_biosettings_avg_years, expand = FALSE, fill = TRUE, padding = 3)         #true
#vbox_biosettings_simu$packStart(lbl_browse_externalRPs, expand = FALSE, fill = TRUE, padding = 3)         #true     
vbox_biosettings_simu$packStart(hbox_biosettings_externalRPs, expand = FALSE, fill = TRUE, padding = 3)         #true     

   gtkWidgetSetSensitive(entry_biosettings_avg_years, F)
           gtkWidgetSetSensitive(lbl_biosettings_avg_years, F) 
           gtkWidgetSetSensitive(bmt_chk_RP_ALADYM_use, F) 
            gtkWidgetSetSensitive(bmt_chk_RP_ALADYM_calc, F) 
            
     gtkWidgetSetSensitive(btn_browse_externalRPs, T) 
         gtkWidgetSetSensitive(lbl_browse_externalRPs, T) 
            gtkWidgetSetSensitive(lbl_bio_externalRP, T) 