reload_EMPTY_M_females <- function(w) {
                           
Mvector_F <<- list()
Mvector_FIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 


   table_F <- data.frame(matrix(0, nrow = length(c(Tr:(n_ages*12))), ncol=2)) 
heading <- c("age_month", "M")
   colnames(table_F) <- heading
   table_F$age_month <- as.character(c(Tr:(n_ages*12)))
   
   mortality.Mvector.females  <<- table_F
   
   add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 
 Mvector_F.treeview$destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)
  
  }
 
 
 
 
 
  
 reload_EMPTY_M_males <- function(w) {
                           
Mvector_M <<- list()
Mvector_MIndex <<- 0

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 


   table_M <- data.frame(matrix(0, nrow = length(c(Tr:(n_ages*12))), ncol=2)) 
heading <- c("age_month", "M")
   colnames(table_M) <- heading
   table_M$age_month <- as.character(c(Tr:(n_ages*12)))
   
   mortality.Mvector.males  <<- table_M
   
   add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter,0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter,2,TRUE)
  } 
 Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)
Mvector_M.sw$add(Mvector_M.treeview)
  
  }