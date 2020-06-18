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
# Add the columns to to be rendered in the tree (MALES)
# ------------------------------------------------------------------------------
#
Mvector_M.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  Mvector_M.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  age_month_frame <- data.frame(c(0))	
  colnames(age_month_frame) <- c("age_month")			       
  renderer$setData("column", age_month_frame)
  treeview$insertColumnWithAttributes(-1, "Age (months)" , renderer, text = 0, editable = FALSE)

  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited",Mvector_M.cell_edited, Mvector_M.model)
  M_frame <- data.frame(c(1))	
  colnames(M_frame) <- c("M")			       
  renderer$setData("column", M_frame)
  treeview$insertColumnWithAttributes(-1, "Natural mortality" , renderer, text = 1, editable = 2)
  
# print("Natural mortality (MALES) column added with success!")
}