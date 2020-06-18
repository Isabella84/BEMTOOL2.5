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
# ------------------------------------------------------------------------------
# add elements to the list of the selectivity values
# ------------------------------------------------------------------------------
#
add.VITpaths_females <- function() {

  if (!is.null(matrix_VITpath) ) {
  VITpaths_femaless <<- matrix_VITpath[,c(1,2)] 
  if ( nrow(VITpaths_femaless) != 0) {
  for (r in 1:nrow(VITpaths_femaless)) {
  dis_temp <- as.list(VITpaths_femaless[r,]) 
    names(dis_temp) <-  c(	"Year", "File")
  VITpaths_females_list <<- c(VITpaths_females_list, list(dis_temp)) 
  }
  }
   }
# print("DISCARD (simulation) list successfully updated!", quote=F)
    #print(selectivities[1])
}