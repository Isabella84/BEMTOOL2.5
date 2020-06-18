# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







addInteractions<-function(w) {
 
  selected_LOAs <- gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(loa.treeview))
  
  selected_SPECIESs <- gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(species.treeview))
  
  if (length(selected_SPECIESs$retval) != 0) {
  spe_idx <- gtkTreePathGetIndices(selected_SPECIESs$retval[[1]])
  selected_spe <- as.character( SPECIESs[spe_idx +1,]) 
  
  selected_GEARs <- gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(gear.treeview))
    
    if (length(selected_GEARs$retval) != 0) {
    gea_idx <- gtkTreePathGetIndices(selected_GEARs$retval[[1]])
  selected_gear <- as.character(GEARs[gea_idx +1,] )



  if (length(selected_LOAs$retval) != 0) {
  for (it in 1:length(selected_LOAs$retval)) {
        loa_idx <- gtkTreePathGetIndices(selected_LOAs$retval[[it]])
        
        new_fleetsegm <- paste(selected_gear, "_", FLEETs[loa_idx +1,], sep="")

         if (!(selected_spe %in% BMT_SPECIES)) {
        BMT_SPECIES <<- c(BMT_SPECIES, selected_spe)
       
        to_add_s <-  data.frame(matrix(c(paste("casestudy.S", (length(BMT_SPECIES)), sep=""), BMT_SPECIES[length(BMT_SPECIES)]), nrow=1))
            colnames(to_add_s) <- colnames(mat_cfg_S) 
            mat_cfg_S <<-  rbind(mat_cfg_S, to_add_s)
       
       }
      
       if (!(new_fleetsegm %in% BMT_FLEETSEGMENTS)) {
          BMT_FLEETSEGMENTS <<- c(BMT_FLEETSEGMENTS, new_fleetsegm)
            to_add_f <-  data.frame(matrix(c(paste("casestudy.F", (length(BMT_FLEETSEGMENTS)), sep=""), BMT_FLEETSEGMENTS[length(BMT_FLEETSEGMENTS)]), nrow=1))
            colnames(to_add_f) <- colnames(mat_cfg_F) 
            mat_cfg_F <<-  rbind(mat_cfg_F, to_add_f)
       }
    
      already_are <- paste(interactions$Species, interactions$Fleet_Segment)
            
      if (!(paste(selected_spe, new_fleetsegm) %in% already_are)) {      
           to_add <- data.frame(matrix(c(selected_spe, new_fleetsegm ), nrow=1, ncol=2))
        colnames(to_add) <- colnames(interactions)
       interactions <<- rbind(interactions, to_add)
       reload_interaction_table() 
      }
  
  }


reload_interaction_table()


nostockss <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostockss) <- "Stocks"

reload_nostocks_table() 

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <- "Fleet segment"

reload_nofleetsegments_table()  
mat_cfg_SF <<- set_mat_cfg_SF(mat_cfg_SF)

gtkEntrySetText(entry_casestudy_NOFLEETSEGMENT, length(BMT_FLEETSEGMENTS))
gtkEntrySetText(entry_casestudy_NOSTOCK, length(BMT_SPECIES) )

  } else {
     showError("You must set at least a LOA for the selected interaction!")
  }

} else {
     showError("You must set the Fishing Technique for the selected interaction(s)!")
}

} else {
      showError("You must set the Species for the selected interaction(s)!")
}
#print(mat_cfg_SF)  
#print(BMT_SPECIES)
#print(mat_cfg_S)
#print(BMT_FLEETSEGMENTS)
#print(mat_cfg_F)
 
# return(mat_cfg_SF)
}
