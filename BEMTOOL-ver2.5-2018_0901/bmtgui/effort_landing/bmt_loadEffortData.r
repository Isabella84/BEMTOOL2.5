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
#
#
#



bmt_loadEffortData <- function(w) {

load_effort_window <<- gtkWindow(show=FALSE)       
load_effort_window["title"] <- "BEMTOOL v2.0 - Effort Data"  
gtkWindowSetModal(load_effort_window, TRUE)    
gtkWindowSetResizable(load_effort_window, FALSE)           
gtkWindowSetDefaultSize(load_effort_window, 400, 20)
gtkWindowSetPosition(load_effort_window, 3)   


btn_browse_load_effort_vessels <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_vessels, "Browse...")
btn_browse_load_effort_vessels$AddCallback("clicked", assign_effort_vessels_path)

btn_browse_load_effort_days <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_days, "Browse...")
btn_browse_load_effort_days$AddCallback("clicked", assign_effort_days_path)

btn_browse_load_effort_gt <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_gt, "Browse...")
btn_browse_load_effort_gt$AddCallback("clicked", assign_effort_gt_path)

btn_browse_load_effort_kw <<- gtkButton()
gtkButtonSetLabel(btn_browse_load_effort_kw, "Browse...")
btn_browse_load_effort_kw$AddCallback("clicked", assign_effort_kw_path)

 entry_effort_vessels_path <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_vessels_path, 70) 
         gtkEntrySetEditable(entry_effort_vessels_path,F)
         
          entry_effort_days_path <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_days_path, 70) 
         gtkEntrySetEditable(entry_effort_days_path,F)
         
          entry_effort_gt_path <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_gt_path, 70) 
         gtkEntrySetEditable(entry_effort_gt_path,F)
         
          entry_effort_kw_path <<-  gtkEntry()
       gtkEntrySetWidthChars(entry_effort_kw_path, 70) 
         gtkEntrySetEditable(entry_effort_kw_path,F)


              gtkEntrySetText(entry_effort_vessels_path,  mat_cfg_EffortData[2,2]) 
                   gtkEntrySetText(entry_effort_days_path,  mat_cfg_EffortData[2,3]) 
                        gtkEntrySetText(entry_effort_gt_path,  mat_cfg_EffortData[2,4]) 
                             gtkEntrySetText(entry_effort_kw_path,  mat_cfg_EffortData[2,5]) 

 tbl_EffortData_load <- gtkTable(rows = 2, columns = 3, homogeneous = FALSE)
 tbl_EffortData_load$SetRowSpacings(7)
 tbl_EffortData_load$SetColSpacings(10)
 tbl_EffortData_load$SetBorderWidth(5)
 
 i=0 # column 0
 j=0
  tbl_EffortData_load$Attach(gtkLabel("Monthly fishing vessels    "),i, i+1, j, j+1)    
  j=j+1
    tbl_EffortData_load$Attach(gtkLabel("Monthly average Days at sea    "),i, i+1, j, j+1)  
      j=j+1
    tbl_EffortData_load$Attach(gtkLabel("Monthly average GT    "),i, i+1, j, j+1)          
       j=j+1
    tbl_EffortData_load$Attach(gtkLabel("Monthly average KW    "),i, i+1, j, j+1)          
     
i=i+1 # column 1
 j=0
   tbl_EffortData_load$Attach(btn_browse_load_effort_vessels,i, i+1, j, j+1) 
    j=j+1
   tbl_EffortData_load$Attach(btn_browse_load_effort_days,i, i+1, j, j+1) 
       j=j+1
   tbl_EffortData_load$Attach(btn_browse_load_effort_gt,i, i+1, j, j+1) 
       j=j+1
   tbl_EffortData_load$Attach(btn_browse_load_effort_kw,i, i+1, j, j+1) 
   
 i=i+1 # column 2
 j=0
   tbl_EffortData_load$Attach(entry_effort_vessels_path,i, i+1, j, j+1)
    j=j+1
   tbl_EffortData_load$Attach(entry_effort_days_path,i, i+1, j, j+1) 
    j=j+1
   tbl_EffortData_load$Attach(entry_effort_gt_path,i, i+1, j, j+1) 
    j=j+1
   tbl_EffortData_load$Attach(entry_effort_kw_path,i, i+1, j, j+1)  




  
ok_button <- gtkButton("     Load data     ")  
ok_button$AddCallback("clicked", set_effort_data_paths)
vbox <- gtkVBox()
hbox1 <- gtkHBox()
hbox1$PackStart(tbl_EffortData_load, expand = TRUE, fill = FALSE, padding = 40)

hbox2 <- gtkHBox(homogeneous = TRUE)
hbox2$PackStart(ok_button, expand = FALSE, fill = FALSE, padding = 100) 
vbox$PackStart(hbox1, expand = TRUE, fill = FALSE, padding = 30)
vbox$PackStart(hbox2, expand = TRUE, fill = FALSE, padding = 20)
load_effort_window$add(vbox)
load_effort_window$show()

}
