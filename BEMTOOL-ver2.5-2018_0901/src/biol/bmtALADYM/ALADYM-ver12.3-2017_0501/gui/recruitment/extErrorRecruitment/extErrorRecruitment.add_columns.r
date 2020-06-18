# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







# ---------------------------------- add the columns to to be rendered in the tree
extErrorRecruitment.add_columns <- function(extErrorRecruitment.treeview) {
#print("Adding column to the model...")   
  extErrorRecruitment.model <- extErrorRecruitment.treeview$getModel()
  for (e in c(1:nrow(CI_external_matrix))) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", extErrorRecruitment.cell_edited, extErrorRecruitment.model)
    offmonthly_frame <- data.frame(c(e-1))	
   # colnames(offmonthly_frame) <- c(MONTHS[e])			       
  renderer$setData("column", offmonthly_frame)
  extErrorRecruitment.treeview$insertColumnWithAttributes(-1, e, renderer, text = (e-1), editable = ncol(CI_external_matrix))
}
}

