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
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
bmt_COSTS.add_columns <- function(treeview) {
# print("Adding column to the model...")   
  bmt_COSTS.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c(" Year ")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, " Year " , renderer, text = 0, editable = FALSE)
  for (e in 1:length(COSTS_vector)) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", bmt_COSTS.cell_edited, bmt_COSTS.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- paste(" ", COSTS_vector_names[e], " ", sep="")				       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, paste(" ", COSTS_vector_names[e], " ", sep=""), renderer, text = e, editable = (length(COSTS_vector)+1))
}
}