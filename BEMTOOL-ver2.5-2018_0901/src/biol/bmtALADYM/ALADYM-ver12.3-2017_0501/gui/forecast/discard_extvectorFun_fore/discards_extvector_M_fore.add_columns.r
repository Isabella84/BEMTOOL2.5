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
discards_extvector_M_fore.add_columns <- function(treeview) {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

first_age_mal <- 0

#if (modulo(Tr, 12) == 0 & Tr!=0) {
#    n_ages <- n_ages - trunc(Tr/12) + 1
#    first_age_mal <- trunc(Tr/12) - 1
#} else if (trunc(Tr/12) > 0 & Tr!=0) {
    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
#} 

#print("Adding column to the model...")   
  discards_extvector_M_fore.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("year")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "Year" , renderer, text = 0, editable = FALSE)
  for (e in 1:n_ages) {
  # number column
  renderer <- gtkCellRendererTextNew()
  gSignalConnect(renderer, "edited", discards_extvector_M_fore.cell_edited, discards_extvector_M_fore.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- c(paste("age", (e+first_age_mal-1), sep=""))			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, as.character(paste("age", (e+first_age_mal-1), sep="")), renderer, text = e, editable = (n_ages+1))
}
}

