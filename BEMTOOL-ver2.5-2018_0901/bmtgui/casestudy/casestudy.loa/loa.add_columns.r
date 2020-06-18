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
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
loa.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  loa.model <- treeview$getModel()
   
  renderer <- gtkCellRendererTextNew()
  #renderer <- gtkCellRendererToggleNew()
  # gSignalConnect(renderer, "toggled", updateInteractions, loa.model)
  param1_frame <- data.frame(c(0))
  param1_name <- " LOA "	
  colnames(param1_frame) <- c(param1_name)			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1, param1_name , renderer, text = 0, editable = FALSE)
  
}
