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
interaction.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  interaction.model <- treeview$getModel()
   
  renderer <- gtkCellRendererTextNew()
  #gSignalConnect(renderer, "edited", interaction.cell_edited, interaction.model)
  param1_frame <- data.frame(c(0))
  param1_name <- " Species "	
  colnames(param1_frame) <- c(param1_name)			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1, param1_name , renderer, text = 0, editable = FALSE)
  
    renderer <- gtkCellRendererTextNew()
#  gSignalConnect(renderer, "edited",interaction.cell_edited, interaction.model)
  param2_frame <- data.frame(c(1))
  param2_name <- " Fleet_Segment "	
  colnames(param2_frame) <- c(param2_name)			       
  renderer$setData("column", param2_frame)
  treeview$insertColumnWithAttributes(-1, param2_name , renderer, text = 1, editable = TRUE)
  

}
