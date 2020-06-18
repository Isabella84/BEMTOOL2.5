# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# aggiungere tutto a vbox_hr_change_effort




# ---------------------------------------------------------------------------------
bmt_entry_timespan_r5 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_timespan_r5, NUMERICAL_ENTRY_LENGTH)

bmt_entry_reduction_r5 <- gtkEntry()
gtkEntrySetWidthChars(bmt_entry_reduction_r5, NUMERICAL_ENTRY_LENGTH)


 bmt_tbl_scenarios_r5_text_input <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_scenarios_r5_text_input$SetRowSpacings(5)
bmt_tbl_scenarios_r5_text_input$SetColSpacings(10)
bmt_tbl_scenarios_r5_text_input$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_scenarios_r5_text_input$Attach(gtkLabel("Time span"),i, i+1, j, j+1) 
j=j+1
bmt_tbl_scenarios_r5_text_input$Attach(gtkLabel("% of reduction"),i, i+1, j, j+1) 

i=i+1 # column 0 
j=0 
bmt_tbl_scenarios_r5_text_input$Attach(bmt_entry_timespan_r5,i, i+1, j, j+1) 
j=j+1
bmt_tbl_scenarios_r5_text_input$Attach(bmt_entry_reduction_r5,i, i+1, j, j+1) 

#vbox_scenarios_r5_text_input <- gtkVBox(FALSE, 5)

#hbox_scenarios_r5_text_input_1 <- gtkHBox(FALSE, 5)
#hbox_scenarios_r5_text_input_1$packStart(gtkLabel("Time span") , expand=F, F, 10) 
#hbox_scenarios_r5_text_input_1$packStart(bmt_entry_timespan_r5 , expand=F, F, 15) 
#hbox_scenarios_r5_text_input_2 <- gtkHBox(FALSE, 5)
#hbox_scenarios_r5_text_input_2$packStart(gtkLabel("% of reduction") , expand=F, F, 15) 
#hbox_scenarios_r5_text_input_2$packStart(bmt_entry_reduction_r5 , expand=F, F,15) 
#hbox_scenarios_r5_text_input_2$packStart(lbl_description_input_r5 , expand=F, F,15) 

#vbox_scenarios_r5_text_input$packStart(hbox_scenarios_r5_text_input_1 , expand=F, F, 5)
#vbox_scenarios_r5_text_input$packStart(hbox_scenarios_r5_text_input_2 , expand=F, F, 5)
#vbox_hr_change_f_by_fleet$packStart(hbox_scenarios_r5_text_input , expand=F, F, 5)

# ------------------------------------------------------------------------------------

# --------------------------- option table
input_table_r5.sw <<- gtkScrolledWindowNew(NULL, NULL)
input_table_r5.sw$setShadowType("etched-in")
input_table_r5.sw$setPolicy("automatic", "automatic")
input_table_r5.sw$SetUsize(600, 80)  
input_table_r5_list <<- list()
input_table_r5Index <<- 0
# create model
if (!is.null(BMT_FLEETSEGMENTS))  {
input_table_r5.create_model()
# create tree view
input_table_r5.treeview <<- gtkTreeViewNewWithModel(input_table_r5.model)
input_table_r5.treeview$setRulesHint(TRUE)
input_table_r5.treeview$getSelection()$setMode("single")
input_table_r5.add_columns(input_table_r5.treeview)
input_table_r5.sw$add(input_table_r5.treeview) 
} 

hbox_scenarios_r5_table <- gtkHBox(FALSE, 5)
hbox_scenarios_r5_table$packStart(bmt_tbl_scenarios_r5_text_input, expand=F, F, 10) 
#hbox_scenarios_r5_table$packStart(vbox_scenarios_r5_text_input, expand=F, F, 10) 

vbox_scenarios_r5_table_option_vert <- gtkVBox(FALSE, 5)
vbox_scenarios_r5_table_option_vert$packStart(input_table_r5.sw , expand=F, F, 0)  

#lbl_description_input_r5 <-  gtkLabel("* Reduction can be 1: Exponential, 2: Linear, 3: Logistic")

bmt_vbox_change_f_legend <- gtkVBox(F, 5) 
bmt_hbox_change_f_legend <- gtkHBox(F, 5) 
bmt_vbox_change_f_legend$packStart( gtkLabel("* Reduction can be") , expand = F, F,0)  
bmt_vbox_change_f_legend$packStart( gtkLabel("1: Exponential,") , expand = F, F,0)  
bmt_vbox_change_f_legend$packStart( gtkLabel("2: Linear,") , expand = F, F,0) 
bmt_vbox_change_f_legend$packStart( gtkLabel("3: Logistic") , expand = F, F,0)  
bmt_hbox_change_f_legend$packStart(bmt_vbox_change_f_legend , expand = F, F, 5)

   
#vbox_scenarios_r5_table_option_vert$packStart(lbl_description_input_r5 , expand=F, F, 0)                                                                   

hbox_scenarios_r5_table$packStart(vbox_scenarios_r5_table_option_vert , expand=T, T, 5) 
hbox_scenarios_r5_table$packStart(bmt_hbox_change_f_legend , expand = F, F, 5) 
  
vbox_hr_change_f_by_fleet$packStart(hbox_scenarios_r5_table , expand = T, fill = FALSE, padding = 5)
 




  

  
