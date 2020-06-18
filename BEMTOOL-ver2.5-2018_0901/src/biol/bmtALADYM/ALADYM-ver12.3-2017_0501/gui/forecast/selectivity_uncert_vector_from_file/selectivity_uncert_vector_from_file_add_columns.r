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
selectivity_uncert_vector_from_file_add_columns <- function(treeview) {
#print("Adding column to the model...")   
  selectivity_uncert_vector_from_file_model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("run_N")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "run_N" , renderer, text = 0, editable = FALSE)
  
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param1")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param1" , renderer, text = 1, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param2")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param2" , renderer, text = 2, editable = TRUE)  
  
     year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param3")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param3" , renderer, text = 3, editable = TRUE)  
  
     year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param4")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param4" , renderer, text = 4, editable = TRUE)  
  
     year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param5")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param5" , renderer, text = 5, editable = TRUE)  
  
     year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("sel_type")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "sel_type" , renderer, text = 6, editable = TRUE)  
  
     year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("fleet")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "fleet" , renderer, text = 7, editable = TRUE)  
   
}
