# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#



bmt_loadEffortData_fore <- function(w) {

load_effort_window_fore <<- gtkWindow(show=FALSE) 
load_effort_window_fore["title"] <- "BEMTOOL v2.0 - Effort Data forecast"  
gtkWindowSetModal(load_effort_window_fore, TRUE)    
gtkWindowSetResizable(load_effort_window_fore, FALSE)           
gtkWindowSetDefaultSize(load_effort_window_fore, 400, 20)
gtkWindowSetPosition(load_effort_window_fore, 3)   


btn_browse_load_effort_vessels_fore <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_vessels_fore, "Browse...")
btn_browse_load_effort_vessels_fore$AddCallback("clicked", assign_effort_vessels_path_fore)

btn_browse_load_effort_days_fore <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_days_fore, "Browse...")
btn_browse_load_effort_days_fore$AddCallback("clicked", assign_effort_days_path_fore)

 entry_effort_vessels_path_fore <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_vessels_path_fore, 70) 
        # gtkEntrySetEditable(entry_effort_vessels_path_fore,F)
         
          entry_effort_days_path_fore <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_days_path_fore, 70) 
        # gtkEntrySetEditable(entry_effort_days_path_fore,F)

         SCEN_EFFS <- c(BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL)   
         
         if (BMT_SCENARIO %in% SCEN_EFFS) {
                            gtkEntrySetText(entry_effort_vessels_path_fore,  mat_cfg_scenario_settings_fore_options[[1]]) 
                   gtkEntrySetText(entry_effort_days_path_fore,  mat_cfg_scenario_settings_fore_options[[1]]) 
         }  else {
                       gtkEntrySetText(entry_effort_vessels_path_fore,  "") 
                   gtkEntrySetText(entry_effort_days_path_fore,  "") 
         }

 tbl_EffortData_load_fore <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_EffortData_load_fore$SetRowSpacings(7)
 tbl_EffortData_load_fore$SetColSpacings(10)
 tbl_EffortData_load_fore$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_EffortData_load_fore$Attach(gtkLabel("Monthly fishing vessels    "),i, i+1, j, j+1)    
  j=j+1
    tbl_EffortData_load_fore$Attach(gtkLabel("Monthly average Days at sea    "),i, i+1, j, j+1)          
     
i=i+1 # column 1
 j=0
   tbl_EffortData_load_fore$Attach(btn_browse_load_effort_vessels_fore,i, i+1, j, j+1) 
    j=j+1
   tbl_EffortData_load_fore$Attach(btn_browse_load_effort_days_fore,i, i+1, j, j+1) 

   
 i=i+1 # column 2
 j=0
   tbl_EffortData_load_fore$Attach(entry_effort_vessels_path_fore,i, i+1, j, j+1)
    j=j+1
   tbl_EffortData_load_fore$Attach(entry_effort_days_path_fore,i, i+1, j, j+1) 





  
ok_button_fore <- gtkButton("     Load data     ")  
ok_button_fore$AddCallback("clicked", set_effort_data_paths_fore)
vbox_fore <- gtkVBox()
hbox1_fore <- gtkHBox()
hbox1_fore$PackStart(tbl_EffortData_load_fore, expand = TRUE, fill = FALSE, padding = 40)

hbox2_fore <- gtkHBox(homogeneous = TRUE)
hbox2_fore$PackStart(ok_button_fore, expand = FALSE, fill = FALSE, padding = 100) 
vbox_fore$PackStart(hbox1_fore, expand = TRUE, fill = FALSE, padding = 30)
vbox_fore$PackStart(hbox2_fore, expand = TRUE, fill = FALSE, padding = 20)
load_effort_window_fore$add(vbox_fore)
load_effort_window_fore$show()

return(load_effort_window_fore)

}
