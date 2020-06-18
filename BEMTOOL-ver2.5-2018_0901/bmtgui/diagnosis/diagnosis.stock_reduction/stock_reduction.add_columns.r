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
stock_reduction.add_columns <- function(treeview) {
# print("Adding column to the model...")   
  stock_reduction.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(0))	
  colnames(col1_frame) <- c(" Stock ")			               
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " Stock " , renderer, text = 0, editable = FALSE)


  renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(1))	
  colnames(col1_frame) <- c(" % needed reduction (last year) ")			                
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " % needed reduction (last year) " , renderer, text = 1, editable = FALSE)


    renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(2))	
  colnames(col1_frame) <- c(" Fcurrent ")			               
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " Fcurrent " , renderer, text = 2, editable = FALSE)


      renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(3))	
  colnames(col1_frame) <- c(" Reference point ")			             
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " Reference point " , renderer, text = 3, editable = FALSE)
  
  
        renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(4))	
  colnames(col1_frame) <- c(" SSBcurrent ")			           
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " SSBcurrent " , renderer, text = 4, editable = FALSE)
  
  
        renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(5))	
  colnames(col1_frame) <- c(" SSBref ")			        
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " SSBref " , renderer, text = 5, editable = FALSE)
  
  
           renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(6))	
  colnames(col1_frame) <- c(" Comments ")			         
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " Comments " , renderer, text = 6, editable = FALSE)

}