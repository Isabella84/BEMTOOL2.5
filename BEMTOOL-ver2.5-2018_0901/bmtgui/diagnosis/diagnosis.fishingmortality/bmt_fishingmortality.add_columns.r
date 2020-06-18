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
bmt_fishingmortality.add_columns <- function(treeview) {
# print("Adding column to the model...")   

 index_to_load = -1
  selected <- gtkComboBoxGetActiveText(bmt_diagnosis_species_pressure)
  index_to_update <- which(BMT_SPECIES == selected)    
names_of_fleets <- mat_cfg_SF[index_to_update,2:ncol(mat_cfg_SF)]

  bmt_fishingmortality.model <- treeview$getModel()
 
  renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(0))	
  colnames(col1_frame) <- c(" Year ")			               
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, " Year " , renderer, text = 0, editable = FALSE)
              

 names_of_cols <-  paste("% F to", names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])
  
      for (nfle in 1:length(names_of_fleets[names_of_fleets != "-" & names_of_fleets != "" ])) { 
  # number column
  renderer <- gtkCellRendererTextNew()
  col1_frame <- data.frame(c(nfle))	
  colnames(col1_frame) <- paste(" ", names_of_cols[nfle], " ", sep="")					               
  renderer$setData("column", col1_frame)
  treeview$insertColumnWithAttributes(-1, paste(" ", names_of_cols[nfle], " ", sep="")	 , renderer, text = nfle, editable = FALSE)
     }


}