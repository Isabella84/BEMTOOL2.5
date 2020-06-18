# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





reload_species_info<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(combo_species)
  
  index_to_update <- which(BMT_SPECIES == selected)
  
if (!is.null(selected)) {
 
 if (!all( is.na(mat_cfg_species_settings[mat_cfg_species_settings[,1] == paste("casestudy.S", index_to_update,".params", sep=""), 2:ncol(mat_cfg_species_settings)] ) ) ) {
 
 this_spe_params <- mat_cfg_species_settings[mat_cfg_species_settings[,1] == paste("casestudy.S", index_to_update,".params", sep=""),] 
  this_spe_ALA <- mat_cfg_ALADYM_sim[mat_cfg_ALADYM_sim[,1] == paste("casestudy.S", index_to_update,".AladymSimulation", sep=""),]
    this_spe_RPs <- mat_cfg_REF_points[mat_cfg_REF_points[,1] == paste("casestudy.referencepoints.S", index_to_update, sep=""),]  
 
# colnames(mat_cfg_species_settings) <- c("", "[female.lifespan]", "[male.lifespan]", "[sexratio]", "[a.female]", "[b.female]", "[a.male]", "[b.male]", "[t0.female]", "[k.female]", "[linf.female]", "[t0.male]", "[k.male]", "[linf.male]", "[l50.female]", "[matrange.female]", "[l50.male]", "[matrange.male]", "[natural mortality constant]", "[stock recruitment relationship]")
#colnames(mat_cfg_ALADYM_sim) <- c("", "[ALADYM simulation]",	"[Average years for RP and forecast]")
#colnames(mat_cfg_REF_points) <- c("","[RP ALADYM calculation]",	"[RP ALADYM use]", "[external table RPs]")
 
  gtkEntrySetText(bmt_entryAB_A_M, this_spe_params[1,colnames(this_spe_params) == "[a.male]"]) 
   
  gtkEntrySetText(bmt_entryAB_A_F, this_spe_params[1,colnames(this_spe_params) == "[a.female]"]) 
  gtkEntrySetText(bmt_entryAB_B_M , this_spe_params[1,colnames(this_spe_params) == "[b.male]"]) 
  gtkEntrySetText(bmt_entryAB_B_F, this_spe_params[1,colnames(this_spe_params) == "[b.female]"]) 

  gtkEntrySetText(bmt_entryVBF_linf_M , this_spe_params[1,colnames(this_spe_params) == "[linf.male]"]) 
  gtkEntrySetText(bmt_entryVBF_linf_F , this_spe_params[1,colnames(this_spe_params) == "[linf.female]"])  
  gtkEntrySetText(bmt_entryVBF_t0_M , this_spe_params[1,colnames(this_spe_params) == "[t0.male]"]) 
  gtkEntrySetText(bmt_entryVBF_t0_F, this_spe_params[1,colnames(this_spe_params) == "[t0.female]"]) 
  gtkEntrySetText(bmt_entryVBF_k_M , this_spe_params[1,colnames(this_spe_params) == "[k.male]"]) 
  gtkEntrySetText(bmt_entryVBF_k_F , this_spe_params[1,colnames(this_spe_params) == "[k.female]"]) 

  gtkEntrySetText(bmt_entryOGIVEL50_M , this_spe_params[1,colnames(this_spe_params) == "[l50.male]"] ) 
  gtkEntrySetText(bmt_entryOGIVEL50_F , this_spe_params[1,colnames(this_spe_params) == "[l50.female]"]) 

  gtkEntrySetText(bmt_entryMR_M , this_spe_params[1,colnames(this_spe_params) == "[matrange.male]"]) 
  gtkEntrySetText(bmt_entryMR_F , this_spe_params[1,colnames(this_spe_params) == "[matrange.female]"] ) 

  gtkEntrySetText(bmt_entryLS_M , this_spe_params[1,colnames(this_spe_params) == "[male.lifespan]"] ) 
  gtkEntrySetText(bmt_entryLS_F , this_spe_params[1,colnames(this_spe_params) == "[female.lifespan]"]) 

  gtkEntrySetText(bmt_entry_sexratio , this_spe_params[1,colnames(this_spe_params) == "[sexratio]"] ) 
 
  if (as.logical(this_spe_params[1,colnames(this_spe_params) == "[natural mortality constant]"])) {
      gtkToggleButtonSetActive(bmt_chk_Mcostant, T)
  } else {
      gtkToggleButtonSetActive(bmt_chk_Mcostant, F)
  } 
 
   if (as.logical(this_spe_params[1,colnames(this_spe_params) == "[stock recruitment relationship]"])) {
      gtkToggleButtonSetActive(bmt_chk_SRrelationship, T)
  } else {
            gtkToggleButtonSetActive(bmt_chk_SRrelationship, F)
  } 
 
  if (as.logical(this_spe_ALA[1,colnames(this_spe_ALA) == "[ALADYM simulation]"]) ) {
      gtkToggleButtonSetActive(bmt_chk_runALADYM, T)
        gtkEntrySetText(entry_biosettings_avg_years , this_spe_ALA[1, colnames(this_spe_ALA) == "[Average years for RP and forecast]"] ) 
  } else {
       gtkToggleButtonSetActive(bmt_chk_runALADYM, F)
  }
  
    if (as.logical(this_spe_RPs[1,colnames(this_spe_RPs) == "[RP ALADYM calculation]"]) ) {
      gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_calc, T)
  }  else {
           gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_calc, F)
  } 
  
      if (as.logical(this_spe_RPs[1,colnames(this_spe_RPs) == "[RP ALADYM use]"]) ) {
      gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_use, T)
  }  else {
   gtkToggleButtonSetActive(bmt_chk_RP_ALADYM_use, F)
      gtkEntrySetText(lbl_bio_externalRP ,this_spe_RPs[1,colnames(this_spe_RPs) == "[external table RPs]"] )  
  } 
 
 mat_temp <<- mat_cfg_assessment_tools[[index_to_update]]
  
  if (!is.null(mat_temp)) {
  which(ASSESSMENT_TOOLS == as.character(mat_temp[2,2]) )
gtkComboBoxSetActive(combo_assessment_tool,   (which(ASSESSMENT_TOOLS == mat_temp[2,2] )-1) )
set_assessment_tool()
}
 
 }  else {
       clear_bio_items()
 }
 
 }
} 
