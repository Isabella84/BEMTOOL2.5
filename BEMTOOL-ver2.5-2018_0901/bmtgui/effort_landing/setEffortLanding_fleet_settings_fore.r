# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


setEffortLanding_fleet_settings_fore <- function(w) {

# temp <- mat_cfg_species_settings

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_effort_r4)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update <- which(BMT_FLEETSEGMENTS == selected)    

EFFORT_NUMBER_list_fore[[index_to_update]] <<- bmt_fleet.NUMBER_r4 
EFFORT_DAY_list_fore[[index_to_update]] <<-  bmt_fleet.DAY_r4  

path <- choose.dir(default = "", caption = "")

save_effort_files_fore(path)

wnd <- showMessageOK(paste("        ", selected, "saved and all fleet segments for forecast exported!        "))


# da sistemare anche il landing

#bmt_fleet.LANDING <<- NULL

}

assign_effort_vessels_path_fore <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_vessels_path_fore, path) 
     }    
}



assign_effort_days_path_fore <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_effort_days_path_fore, path) 
     }    
}