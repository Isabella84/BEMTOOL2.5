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
selectivity_uncert_distribution_from_file_add_columns <- function(treeview) {
#print("Adding column to the model...")   
  selectivity_uncert_distribution_from_file_model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("fleet")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "fleet" , renderer, text = 0, editable = FALSE)
  
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("sel_type")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "sel_type" , renderer, text = 1, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("distribution")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "distribution" , renderer, text = 2, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param1_a")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param1_a" , renderer, text = 3, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param2_a")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param2_a" , renderer, text = 4, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param3_a")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param3_a" , renderer, text = 5, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param4_a")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param4_a" , renderer, text = 6, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param5_a")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param5_a" , renderer, text = 7, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param1_b")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param1_b" , renderer, text = 8, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param2_b")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param2_b" , renderer, text = 9, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param3_b")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param3_b" , renderer, text = 10, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param4_b")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param4_b" , renderer, text = 11, editable = TRUE)  
  
   year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("param5_b")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "param5_b" , renderer, text = 12, editable = TRUE)  
}
