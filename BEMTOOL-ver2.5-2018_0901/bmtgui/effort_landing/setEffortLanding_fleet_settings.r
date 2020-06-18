# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


  

setEffortLanding_fleet_settings <- function(w) {
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_economicdata_fleet_combo)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update <- which(BMT_FLEETSEGMENTS == selected)    

EFFORT_NUMBER_list[[index_to_update]] <<- bmt_fleet.NUMBER
EFFORT_DAY_list[[index_to_update]] <<-  bmt_fleet.DAY 
EFFORT_GT_list[[index_to_update]] <<-  bmt_fleet.GT
EFFORT_KW_list[[index_to_update]] <<-  bmt_fleet.KW 

LANDING_list_all[[index_to_update]] <<- LANDING_list

path <- choose.dir(default = "", caption = "")

save_effort_files(path)

wnd <- showMessageOK(paste("        ", selected, "saved and all fleet segments exported!        "))


# da sistemare anche il landing

#bmt_fleet.LANDING <<- NULL

}


assign_effort_vessels_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_vessels_path, path) 
     }    
}


assign_effort_days_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_days_path, path) 
     }    
}


assign_effort_gt_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_gt_path, path) 
     }    
}

assign_effort_kw_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_kw_path, path) 
     }    
}



assign_species_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
      
      index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_eco_landing_species)
index_to_update <- which(BMT_SPECIES == selected)       

mat_temp <- mat_cfg_LandingData

levels(mat_temp[,(index_to_update+1)]) <- factor(c(levels(mat_temp[,(index_to_update+1)]), path)) 
mat_temp[2,(index_to_update+1)] <-  path

     mat_cfg_LandingData <<- mat_temp
     set_landing_data_lists()
     bmt_reload_landing_species_info()
 
   wnd <- showMessageOK(paste("        ", selected, "landing saved!        "))
     }    
}
