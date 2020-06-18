# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed  to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






change_tr<- function(w) {

go_on_tr <- TRUE
 Tr <<- as.numeric(as.character(gtkEntryGetText(entryOFFSPRING_tr))) 

if (Tr >= as.numeric(min(gtkEntryGetText(entryVBF_M_lifespan) ,gtkEntryGetText(entryVBF_F_lifespan) ))*12 ) { 
   go_on_tr <- FALSE
   showError("Tr must be lower than the minimum indicated life span!")
   }
   
   if (go_on_tr) {
  
 # print(paste("changed TR!", Tr))
 months_vec_M <<- c(Tr:(biological.lifeSpanM*12))
biological.months_MM <<- length(months_vec_M)

 months_vec_F <<- c(Tr:(biological.lifeSpanF*12))
biological.months_MF <<- length(months_vec_F)
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
# if (exists("mortality_temp_M")) { rm(mortality_temp_M) }
#if (is.na(Populations[[ALADYM_spe]]@M.cost$M[1]) ) {
#
#for (nr in 1:nrow(Populations[[ALADYM_spe]]@M.vect$M)) {
#   if (nr==1 ) {
#       mortality_temp_M <- as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$M[nr,] ))
#   } else {
#       mortality_temp_M <- c(mortality_temp_M, as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$M[nr,] )) )
#   }
#}
#
#all_mortality <-  c(mortality_temp_M[1], mortality_temp_M)
#
#mortality_dataframe_M <- data.frame(cbind(seq(Tr, length(mortality_temp_M), 1), all_mortality[(Tr+1):(length(all_mortality))]) )
#colnames(mortality_dataframe_M) <- c("age_month",	"M")
#mortality.Mvector.males <<- mortality_dataframe_M 
#
#mortality.Mvector.males <<- mortality.Mvector.males[which(as.numeric(as.character(mortality.Mvector.males$age_month)) >= Tr),]
#}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


#------------------------------------------ load the file
Mvector_M <<- list()
Mvector_MIndex <<- 0
add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter, 0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter, 2,TRUE)
  } 

   Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)
#Mvector_M.sw$destroy()
#Mvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
#Mvector_M.sw$setShadowType("etched-in")
#Mvector_M.sw$setPolicy("automatic", "automatic")
#Mvector_M.sw$SetUsize(100, 80)  
Mvector_M.sw$add(Mvector_M.treeview)
#vboxM_M$packStart(Mvector_M.sw , TRUE, TRUE, 0)
#
#gtkBoxReorderChild(vboxM_M, Mvector_M.sw, 3)
#gtkBoxReorderChild(vboxM_M, hboxMvector_Mfile_save, 3)
#

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
# if (exists("mortality_temp_F")) { rm(mortality_temp_F) }
#if (is.na(Populations[[ALADYM_spe]]@M.cost$M[2]) ) {
#
#for (nr in 1:nrow(Populations[[ALADYM_spe]]@M.vect$F)) {
#   if (nr==1) {
#       mortality_temp_F <- as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$F[nr,] ))
#   } else {
#       mortality_temp_F <- c(mortality_temp_F, as.numeric(as.character(Populations[[ALADYM_spe]]@M.vect$F[nr,] )) )
#   }
#}
#
#all_mortality <-  c(mortality_temp_F[1], mortality_temp_F)
#
#mortality_dataframe_F <- data.frame(cbind(seq(Tr, length(mortality_temp_F), 1), all_mortality[(Tr+1):(length(all_mortality))] ) )
#colnames(mortality_dataframe_F) <- c("age_month",	"M")
#mortality.Mvector.females <<- mortality_dataframe_F 
#
#mortality.Mvector.females <<- mortality.Mvector.females[which(as.numeric(as.character(mortality.Mvector.females$age_month)) >= Tr),]
#}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#------------------------------------------ load the file
Mvector_F <<- list()
Mvector_FIndex <<- 0
add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 

   Mvector_F.treeview $destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
#Mvector_F.sw$destroy()
#Mvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
#Mvector_F.sw$setShadowType("etched-in")
#Mvector_F.sw$setPolicy("automatic", "automatic")
#Mvector_F.sw$SetUsize(100, 80)  
Mvector_F.sw$add(Mvector_F.treeview)
#vboxM_F$packStart(Mvector_F.sw , TRUE, TRUE, 0)
#
#gtkBoxReorderChild(vboxM_F, Mvector_F.sw, 3)
#gtkBoxReorderChild(vboxM_F, hboxMvector_Ffile_save, 3)
#

}
}