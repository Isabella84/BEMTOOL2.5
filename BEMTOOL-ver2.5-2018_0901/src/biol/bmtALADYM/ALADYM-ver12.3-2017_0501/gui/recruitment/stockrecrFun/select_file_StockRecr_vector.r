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
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of stock-recruitment are 
# saved and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_StockRecr_vector <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
stockr_file <<- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading recruitment...        ")

stockrecruitment.SRvector <<- read.csv(stockr_file, sep=";")
stockrecruitment.SRvector.seed <<- stockrecruitment.SRvector$seed[1] 
stockrecruitment.SRvector <<- stockrecruitment.SRvector[,colnames(stockrecruitment.SRvector) != "seed"] 

#if (nchar(stockr_file) < 57)  {
#  lbl_SRvector_File_txt <- stockr_file
#} else {
#  lbl_SRvector_File_txt <- substring(stockr_file, (nchar(stockr_file)-57), nchar(stockr_file))
#}
#print(lbl_SRvector_File_txt)
#gtkLabelSetText(lbl_SRvectorFile, paste("...", lbl_SRvector_File_txt, sep=""))

check_ <- check_input("RECRUITMENT_VECTOR", stockrecruitment.SRvector)

if (check_$result == "KO") {
    showError(check_$msg)
} else { 
#------------------------------------------ load the file

gtkEntrySetText(entry_OFFSPRING_seedvalue, stockrecruitment.SRvector.seed)
recruitments <<- list()
recruitmentIndex <<- 0
add.recruitments()
  recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments)) {
    iter <-  recruitments.model$append()$iter
     recruitments.model$set(iter,0, recruitments[[i]]$year)
    #print(paste("in model:", as.character(recruitments[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments.model$set(iter, e, as.double(recruitments[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
       #print(paste("in model:", recruitments[[i]][e]) )
    }
     recruitments.model$set(iter,13,TRUE)
  } 

  recruitments.treeview$destroy()

 recruitments.treeview <<- gtkTreeViewNewWithModel( recruitments.model)
 recruitments.treeview$setRulesHint(TRUE)
 recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns( recruitments.treeview)
#recruitment.sw$destroy()
#recruitment.sw <<- gtkScrolledWindowNew(NULL, NULL)
#recruitment.sw$setShadowType("etched-in")
#recruitment.sw$setPolicy("automatic", "automatic")
#recruitment.sw$SetUsize(100, dim_eff_tables)  
recruitment.sw$add(recruitments.treeview)
#vboxSRrelationship$packStart(recruitment.sw , TRUE, TRUE, 0)
#
#gtkBoxReorderChild(vboxSRrelationship, recruitment.sw, 4)
#gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)

}


wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment loaded!        ")


} 
} 
