# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
#
#
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of total mortality are 
# saved and the loading of those values in the table (MALES)
# ------------------------------------------------------------------------------
#
select_file_Zvector <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
totalmortality_file <<- dialog$getFilename() 

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading total mortality...        ")

mortality.Zvector <<- read.csv(totalmortality_file, sep=";")

# mortality.Zvector <<- read.csv("C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\input\\M e F - Z-MERMER.csv", sep=";", na.strings = "")
mortality.Zvector.males <<- mortality.Zvector[mortality.Zvector$sex=="M",colnames(mortality.Zvector) != "sex"]
mortality.Zvector.females <<- mortality.Zvector[mortality.Zvector$sex=="F",colnames(mortality.Zvector) != "sex"]

mortality.Zvector.males.seed <- mortality.Zvector.males$seed[1] 
mortality.Zvector.males <<- mortality.Zvector.males[,colnames(mortality.Zvector.males) != "seed"] 

mortality.Zvector.females.seed <- mortality.Zvector.females$seed[1] 
mortality.Zvector.females <<- mortality.Zvector.females[,colnames(mortality.Zvector.females) != "seed"] 

 check_f <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.females )
 
  check_m <- check_input("TOTAL_MORTALITY_VECTOR", mortality.Zvector.males)

if (check_f$result == "KO") {
     showError(check_$msg)
} else if (check_m$result == "KO") {
     showError(check_$msg)
} else {  
#------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_M, mortality.Zvector.males.seed)
Zvector_M <<- list()
Zvector_MIndex <<- 0
add.Zvector_M()
  Zvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_M)) {
    iter <-  Zvector_M.model$append()$iter
     Zvector_M.model$set(iter,0, Zvector_M[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_M.model$set(iter, e, as.double(Zvector_M[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     Zvector_M.model$set(iter,13,TRUE)
  } 

   Zvector_M.treeview$destroy()
 Zvector_M.treeview <<- gtkTreeViewNewWithModel( Zvector_M.model)
 Zvector_M.treeview$setRulesHint(TRUE)
 Zvector_M.treeview$getSelection()$setMode("single")
Zvector_M.add_columns( Zvector_M.treeview)

Zvector_M.sw$add(Zvector_M.treeview)


#------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_F, mortality.Zvector.females.seed)
Zvector_F <<- list()
Zvector_FIndex <<- 0
add.Zvector_F()
  Zvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_F)) {
    iter <-  Zvector_F.model$append()$iter
     Zvector_F.model$set(iter,0, Zvector_F[[i]]$year)
    #print(paste("in model:", as.character(Zvector_F[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_F.model$set(iter, e, as.double(Zvector_F[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
     #  print(paste("in model:", Zvector_F[[i]][e]) )
    }
     Zvector_F.model$set(iter,13,TRUE)
  } 

  Zvector_F.treeview$destroy()
 Zvector_F.treeview <<- gtkTreeViewNewWithModel( Zvector_F.model)
 Zvector_F.treeview$setRulesHint(TRUE)
 Zvector_F.treeview$getSelection()$setMode("single")
Zvector_F.add_columns( Zvector_F.treeview)
Zvector_F.sw$add(Zvector_F.treeview)

}

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Total mortality loaded!        ")

} 
}
