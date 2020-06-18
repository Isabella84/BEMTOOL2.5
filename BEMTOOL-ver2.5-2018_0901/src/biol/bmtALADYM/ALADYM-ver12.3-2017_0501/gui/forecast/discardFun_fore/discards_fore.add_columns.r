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
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
discards_fore.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  discards_fore.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("year")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "Year" , renderer, text = 0, editable = FALSE)

  # number column
  renderer <- gtkCellRendererTextNew()
  month_frame <- data.frame(c(1))	
  colnames(month_frame) <- c("month")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, "Month" , renderer, text = 1, editable = FALSE)
   
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited",discards_fore.cell_edited, discards_fore.model)
  param1_frame <- data.frame(c(2))
  param1_name <- "L50%"	
  colnames(param1_frame) <- c(param1_name)			       
  renderer$setData("column", param1_frame)
  treeview$insertColumnWithAttributes(-1, param1_name , renderer, text = 2, editable = 4)
  
    renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", discards_fore.cell_edited, discards_fore.model)
  param2_frame <- data.frame(c(3))
  param2_name <- "L75%-L25%"
  colnames(param2_frame) <- c(param2_name)		       
  renderer$setData("column", param2_frame)
  treeview$insertColumnWithAttributes(-1, param2_name , renderer, text = 3, editable = 4)
  #print("Discard forecast column added with success!")
}