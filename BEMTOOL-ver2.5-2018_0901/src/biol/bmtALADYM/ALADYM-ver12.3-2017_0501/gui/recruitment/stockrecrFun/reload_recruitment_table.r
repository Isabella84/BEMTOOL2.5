# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
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
# Function to reload the values for the recruitment according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_recruitment_table<- function(w) {
  #stockrecruitment.SRvector   <<- NULL
  stockrecruitment.SRvector.seed <- gtkEntryGetText(entry_OFFSPRING_seedvalue)
  recruitments <<- list()
recruitmentsIndex <<- 0

   sr_matrix <- data.frame(matrix(as.double(stockrecruitment.SRvector.seed), nrow=length(years), ncol=13))
   colnames(sr_matrix) <- c("year",MONTHS)
     sr_matrix$year <- years
   for (r in 1:nrow(sr_matrix)) { 
  sr_temp <- as.list(sr_matrix[r,]) 
  recruitments <<- c(recruitments, list(sr_temp)) 
  }
  
  stockrecruitment.SRvector <<- sr_matrix

 recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments)) {
    iter <-  recruitments.model$append()$iter
     recruitments.model$set(iter,0, recruitments[[i]]$year)
    #print(paste("in model:", as.character(recruitments[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments.model$set(iter, e, as.double(recruitments[[i]][e+1]))          
       #print(paste("in model:", recruitments[[i]][e]) )
    }
     recruitments.model$set(iter,13,TRUE)
  } 

 recruitments.treeview <<- gtkTreeViewNewWithModel( recruitments.model)
 recruitments.treeview$setRulesHint(TRUE)
 recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns( recruitments.treeview)
recruitment.sw$destroy()
recruitment.sw <<- gtkScrolledWindowNew(NULL, NULL)
recruitment.sw$setShadowType("etched-in")
recruitment.sw$setPolicy("automatic", "automatic")
recruitment.sw$SetUsize(100, dim_eff_tables)  
recruitment.sw$add(recruitments.treeview)
vboxSRrelationship$packStart(recruitment.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vboxSRrelationship, recruitment.sw, 4)
gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)
    
}
