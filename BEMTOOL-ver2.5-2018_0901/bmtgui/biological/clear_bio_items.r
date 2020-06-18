# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


  clear_bio_items <- function() {
 
  gtkEntrySetText(bmt_entryAB_A_M, "") 
   
  gtkEntrySetText(bmt_entryAB_A_F, "") 
  gtkEntrySetText(bmt_entryAB_B_M ,"") 
  gtkEntrySetText(bmt_entryAB_B_F,"") 

  gtkEntrySetText(bmt_entryVBF_linf_M,"") 
  gtkEntrySetText(bmt_entryVBF_linf_F ,"") 
  gtkEntrySetText(bmt_entryVBF_t0_M,"") 
  gtkEntrySetText(bmt_entryVBF_t0_F,"") 
  gtkEntrySetText(bmt_entryVBF_k_M ,"")  
  gtkEntrySetText(bmt_entryVBF_k_F ,"") 

  gtkEntrySetText(bmt_entryOGIVEL50_M,"") 
  gtkEntrySetText(bmt_entryOGIVEL50_F ,"") 

  gtkEntrySetText(bmt_entryMR_M,"") 
  gtkEntrySetText(bmt_entryMR_F,"") 

  gtkEntrySetText(bmt_entryLS_M,"") 
  gtkEntrySetText(bmt_entryLS_F ,"")  

  gtkEntrySetText(bmt_entry_sexratio ,"")  
 
      gtkToggleButtonSetActive(bmt_chk_Mcostant, F)
      gtkToggleButtonSetActive(bmt_chk_SRrelationship, F)
      gtkToggleButtonSetActive(bmt_chk_runALADYM, F)
        gtkEntrySetText(entry_biosettings_avg_years , "" ) 
     
      gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_calc, F)
       gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_use, F)
         gtkEntrySetText(lbl_bio_externalRP ,"")  

         clear_bio_assessment_items()

}
 
  