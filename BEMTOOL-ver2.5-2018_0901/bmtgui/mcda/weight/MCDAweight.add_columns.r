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
MCDAweight.add_columns <- function(treeview) {
# print("Adding column to the model...")   
  MCDAweight.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model) 
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c(" SuperDimension ")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, " SuperDimension " , renderer, text = 0, editable = FALSE)
  
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", MCDAweight.cell_edited, MCDAweight.model)
    month_frame <- data.frame(c(1))	
    colnames(month_frame) <- c(" Dimension ")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, " Dimension "	, renderer, text = 1, editable = 4)
  
    # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", MCDAweight.cell_edited, MCDAweight.model)
    month_frame <- data.frame(c(2))	
    colnames(month_frame) <- c(" Name ")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, " Name "	, renderer, text = 2, editable = 4)
  
    # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", MCDAweight.cell_edited, MCDAweight.model)
    month_frame <- data.frame(c(3))	
    colnames(month_frame) <- c(" Value ")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, " Value "	, renderer, text = 3, editable = 4)

}