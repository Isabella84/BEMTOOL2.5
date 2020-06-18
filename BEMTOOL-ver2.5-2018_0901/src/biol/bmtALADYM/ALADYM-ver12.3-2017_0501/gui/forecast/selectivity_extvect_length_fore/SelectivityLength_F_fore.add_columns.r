# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#
# ------------------------------------------------------------------------------
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
SelectivityLength_F_fore.add_columns <- function(treeview) {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 l_inf <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max))  
} else {
   l_inf <- as.numeric(new_aldPopulation@growth[6,3])      
} 

l_inf_lens <- c(0:(round(l_inf,0)+1))


#print("Adding column to the model...")   
  SelectivityLength_F_fore.model <- treeview$getModel()
  # number column
  
    renderer <- gtkCellRendererTextNew()
    month_frame <- data.frame(c(0))	
    colnames(month_frame) <- c("Length")			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, c(" Length [mm]"), renderer, text = 0, editable = FALSE)
  
  for (e in 1:length(years_forecast)) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", SelectivityLength_F_fore.cell_edited, SelectivityLength_F_fore.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- c(years_forecast[e])			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, as.character(years_forecast[e]), renderer, text = e, editable = (years_forecast))
}
}

