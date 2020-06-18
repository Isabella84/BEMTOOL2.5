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
bmt_labour_commercial.add_columns <- function(treeview) {
# print("Adding column to the model...")   
  bmt_labour_commercial.model <- treeview$getModel()
  # number column
  #renderer <- gtkCellRendererTextNew()
#  # gSignalConnect(renderer, "edited", cell.edited, model)
#  year_frame <- data.frame(c(0))	
#  colnames(year_frame) <- c(" ")			       
#  renderer$setData("column", year_frame)
#  treeview$insertColumnWithAttributes(-1, " " , renderer, text = 0, editable = FALSE) 
 
  for (e in 1:length(BMT_FLEETSEGMENTS)) {
  # number column
 renderer <- gtkCellRendererToggle()
  gSignalConnect(renderer, "toggled", bmt_labour_commercial.cell_edited, bmt_labour_commercial.model) 
 
 if (length(bmt_labour_commercial_list) != 0) {
    gtkCellRendererToggleSetActive(renderer,  bmt_labour_commercial_list[[1]][e][[1]])
 } 
#    if (value) {
#              bmt_labour_commercial.model$set(iter, e, TRUE)
#                gtkCellRendererToggleSetActive(renderer, T)
#         } else {
#              bmt_labour_commercial.model$set(iter, e, FALSE)
#              gtkCellRendererToggleSetActive(renderer, F)
#         }
  
# column <- treeview$getColumn(e)
#   column$setClickable(TRUE)		
    month_frame <- data.frame(c(e-1))	
    colnames(month_frame) <- paste(" ", BMT_FLEETSEGMENTS[e], " ", sep="")				       
  renderer$setData("column", month_frame)                                                                               # indicator-size = 2, toggled = e, editable = (e+1)
  treeview$insertColumnWithAttributes(-1, paste(" ", BMT_FLEETSEGMENTS[e], " ", sep="")	, renderer  )
  
}

}