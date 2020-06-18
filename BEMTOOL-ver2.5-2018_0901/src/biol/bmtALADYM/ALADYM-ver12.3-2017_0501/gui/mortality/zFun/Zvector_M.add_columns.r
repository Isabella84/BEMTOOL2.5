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
#
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
Zvector_M.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  Zvector_M.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("year")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "Year" , renderer, text = 0, editable = FALSE)
  for (e in 1:length(MONTHS)) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", Zvector_M.cell_edited, Zvector_M.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- c(MONTHS[e])			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, as.character(MONTHS[e]), renderer, text = e, editable = 13)
}
}