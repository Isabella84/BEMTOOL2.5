# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ---------------------------------- add the columns to to be rendered in the tree
monthly.survivability.add_columns <- function(monthly.survivability.treeview) {
#print("Adding column to the model...")   
  monthly.survivability.model <- monthly.survivability.treeview$getModel()
  for (e in c(1:length(MONTHS))) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", monthly.survivability.cell_edited, monthly.survivability.model)
    offmonthly_frame <- data.frame(c(e-1))	
    colnames(offmonthly_frame) <- c(MONTHS[e])			       
  renderer$setData("column", offmonthly_frame)
  monthly.survivability.treeview$insertColumnWithAttributes(-1, MONTHS[e], renderer, text = (e-1), editable = 12)
}
}

