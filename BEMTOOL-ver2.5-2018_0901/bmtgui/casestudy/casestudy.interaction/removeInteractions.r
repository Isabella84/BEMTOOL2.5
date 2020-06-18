# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


removeInteractions<-function(w) {
 
  selected_inters <- gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(interaction.treeview))
  
    inter_idxs <- c()

  if (length(selected_inters$retval) != 0) {
  for (it in 1:length(selected_inters$retval)) {
        
        inter_idxs <- c(inter_idxs, gtkTreePathGetIndices(selected_inters$retval[[it]]) )
   }

        print(paste("Removing", interactions[ inter_idxs+1,], "..."))
        interactions <<- interactions[- c(inter_idxs+1), ]

        idx_to_rem <- which(!(mat_cfg_S[,2] %in% as.character(unique(interactions$Species)) ) )
        
        if (length(idx_to_rem) != 0) {
         mat_cfg_S <<-  mat_cfg_S[-idx_to_rem, ]
          BMT_SPECIES <<- BMT_SPECIES[-idx_to_rem]
        }
                if (nrow(mat_cfg_S) != 0) {
                mat_cfg_S[,1] <- paste(paste("casestudy.S", 1:(length(BMT_SPECIES)), sep="")) 
                }
                
          idx_to_rem <- which(!(mat_cfg_F[,2] %in% as.character(unique(interactions$Fleet_Segment)) ) )
        
        if (length(idx_to_rem) != 0) {
          mat_cfg_F <<- mat_cfg_F[-idx_to_rem, ]
          BMT_FLEETSEGMENTS <<- BMT_FLEETSEGMENTS[-idx_to_rem]
        }
                        if (nrow(mat_cfg_F) != 0) {
            mat_cfg_F[,1] <- paste(paste("casestudy.F", 1:(length(BMT_FLEETSEGMENTS)), sep=""))
             }
             
  reload_interaction_table() 
 
  
  if (nrow(interactions) != 0) {
mat_cfg_SF <<- data.frame(matrix( nrow=nrow(mat_cfg_S), ncol=(nrow(mat_cfg_F)+1) ))
colnames(mat_cfg_SF) <- c("", 	paste("[F", c(1:nrow(mat_cfg_F)), "]", sep="") )
mat_cfg_SF[,1] <- paste("casestudy.S", c(1:length(BMT_SPECIES)), ".associatedFleetsegment", sep="")

for (nr in 1:nrow(mat_cfg_S)) {
    fleetss <- as.character(interactions$Fleet_Segment[interactions$Species == BMT_SPECIES[nr] ] )
    mat_cfg_SF[nr, which(BMT_FLEETSEGMENTS %in%  fleetss)+1 ] <-  BMT_FLEETSEGMENTS[which(BMT_FLEETSEGMENTS %in%  fleetss)]
}

 mat_cfg_SF[is.na(mat_cfg_SF[])] <<- ""

mat_cfg_general[mat_cfg_general[,1] == "casestudy.stockno",2] <<-  length(BMT_SPECIES)
mat_cfg_general[mat_cfg_general[,1] == "casestudy.fleetsegmentno",2] <<-  length(BMT_FLEETSEGMENTS)
 mat_cfg_general[is.na(mat_cfg_general[])] <<- ""
 
}

nostockss <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostockss) <- "Stocks"

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <- "Fleet segment"

for (spsp in 1:length(BMT_SPECIES) ) {
    to_add <- data.frame( BMT_SPECIES[spsp])
     colnames(to_add)  <- "Stocks"
    nostockss <<- rbind(nostockss, to_add)
}
    
for (flfl in 1:length(BMT_FLEETSEGMENTS) ) {
      to_add <- data.frame( BMT_FLEETSEGMENTS[flfl])
     colnames(to_add)  <-  "Fleet segment"
    nofleetsegments <<- rbind(nofleetsegments, to_add)
}

reload_nostocks_table() 
reload_nofleetsegments_table() 


print(mat_cfg_SF)
  
  } else {
    wnd <- showMessageOK("Select the interaction to be removed!")
  # errore
  }
  
   print(BMT_SPECIES)
   print(mat_cfg_S)
   print(BMT_FLEETSEGMENTS)
   print(mat_cfg_F)

 
}