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
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
VITpaths_females.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  VITpaths_females.model <- treeview$getModel()
  # number column
renderer <- gtkCellRendererTextNew()
param1_frame <- data.frame(c(0))	
colnames(param1_frame) <- c(" Year ")			       
 renderer$setData("column", param1_frame)
 treeview$insertColumnWithAttributes(-1, " Year " , renderer, text = 0, editable = FALSE)

  # number column
renderer <- gtkCellRendererTextNew()
param2_frame <- data.frame(c(1))	
colnames(param2_frame) <- c(" File ")			       
 renderer$setData("column", param2_frame)
 treeview$insertColumnWithAttributes(-1, " File " , renderer, text = 1, editable = FALSE)
}
