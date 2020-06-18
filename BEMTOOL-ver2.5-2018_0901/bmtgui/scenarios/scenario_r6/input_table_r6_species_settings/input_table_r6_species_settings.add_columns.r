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
input_table_r6_species_settings.add_columns <- function(treeview) {
# print("Adding column to the model...")   
  input_table_r6_species_settings.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("species")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, " Species " , renderer, text = 0, editable = FALSE)
  fields_temp <- c( "% landing/catch", "SRR model *", "F setting **", "SSR Y/N", "Average years")
  for (e in 1:length(fields_temp)) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", input_table_r6_species_settings.cell_edited, input_table_r6_species_settings.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- paste(" ", fields_temp[e], " ", spe="")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, paste(" ", fields_temp[e], " ", sep=""), renderer, text = e, editable = 6)
}
}