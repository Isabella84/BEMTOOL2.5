# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
selectivities_fore.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  selectivities_fore.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("year")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, " Year " , renderer, text = 0, editable = FALSE)
  
  
  # number column
  renderer <- gtkCellRendererTextNew()
  month_frame <- data.frame(c(1))	
  colnames(month_frame) <- c("month")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, " Month " , renderer, text = 1, editable = FALSE)

#  gSignalConnect(renderer, "edited",selectivities_fore.cell_edited, selectivities_fore.model)
  
   renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(2))
  colnames(param1_frame) <- c(" 1st param ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 2, editable = 8)
  
   renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(3))
  colnames(param1_frame) <- c(" 2nd param ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 3, editable = 8)
  
     renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(4))
  colnames(param1_frame) <- c(" 3rd param ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 4, editable = 8)
  
     renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(5))
  colnames(param1_frame) <- c(" 4th param ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 5, editable = 8)
  
     renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(6))
  colnames(param1_frame) <- c(" 5th param ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 6, editable = 8)
  
     renderer <- gtkCellRendererTextNew()   
   gSignalConnect(renderer, "edited", selectivities_fore.cell_edited, selectivities_fore.model)
  param1_frame <- data.frame(c(7))
  colnames(param1_frame) <- c(" Selectivity type ")			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1,  colnames(param1_frame) , renderer, text = 7, editable = 8)
}
