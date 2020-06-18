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
stockrecruitment.SRvector <<- read.csv(dialog$getFilename(), sep=";")
stockrecruitment.SRvector.seed <<- stockrecruitment.SRvector$seed[1] 
stockrecruitment.SRvector <<- stockrecruitment.SRvector[,colnames(stockrecruitment.SRvector) != "seed"] 

#if (nchar(dialog$getFilename()) < 57)  {
#  lbl_SRvector_File_txt <- dialog$getFilename()
#} else {
#  lbl_SRvector_File_txt <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
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

 recruitments.treeview <<- gtkTreeViewNewWithModel( recruitments.model)
 recruitments.treeview$setRulesHint(TRUE)
 recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns( recruitments.treeview)
recruitment.sw$destroy()
recruitment.sw <<- gtkScrolledWindowNew(NULL, NULL)
recruitment.sw$setShadowType("etched-in")
recruitment.sw$setPolicy("automatic", "automatic")
recruitment.sw$SetUsize(100, 80)  
recruitment.sw$add(recruitments.treeview)
vboxSRrelationship$packStart(recruitment.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vboxSRrelationship, recruitment.sw, 4)
gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)

}
dialog$destroy()
} else {
dialog$destroy()
}
} 

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
# add elements to the list of stock-recruitment values
# ------------------------------------------------------------------------------
#
add.recruitments <- function() {
#print("Adding elements to the list...")   
  if (!is.null(stockrecruitment.SRvector)) {
  for (r in 1:nrow(stockrecruitment.SRvector)) {
  sr_temp <- as.list(stockrecruitment.SRvector[r,]) 
  names(sr_temp) <- c("year",MONTHS)
  recruitments <<- c(recruitments, list(sr_temp)) 
  }
   } else {
   sr_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(sr_matrix) <- c("year",MONTHS)
     sr_matrix$year <- years
   for (r in 1:nrow(sr_matrix)) { 
  sr_temp <- as.list(sr_matrix[r,]) 
  recruitments <<- c(recruitments, list(sr_temp)) 
  }
 }
 print("Recruitments list successfully updated!", quote=F)
}

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
# create model for the tree of stock-recruitment
# ------------------------------------------------------------------------------
#
recruitments.create_model <- function() {
#print("Creating model...")   
  # create list store
  recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  add.recruitments()
  # add items 
  for (i in 1:length(recruitments)) {
    iter <- recruitments.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    recruitments.model$set(iter,0, recruitments[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         recruitments.model$set(iter, e, as.numeric(recruitments[[i]][e+1]))
    }
       recruitments.model$set(iter, 13,TRUE)
  } 
  #print("Recruitment Model successfully created!")  
}

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
# Add the columns to to be rendered in the tree
# ------------------------------------------------------------------------------
#
recruitments.add_columns <- function(treeview) {
#print("Adding column to the model...")   
  recruitments.model <- treeview$getModel()
  # number column
  renderer <- gtkCellRendererTextNew()
  # gSignalConnect(renderer, "edited", cell.edited, model)
  year_frame <- data.frame(c(0))	
  colnames(year_frame) <- c("year")			       
  renderer$setData("column", year_frame)
  treeview$insertColumnWithAttributes(-1, "Year" , renderer, text = 0, editable = FALSE)
  for (e in 1:length(MONTHS)) {
  # number column
  renderer <- gtkCellRendererTextNew()                            
  gSignalConnect(renderer, "edited", recruitments.cell_edited, recruitments.model)
    month_frame <- data.frame(c(e))	
    colnames(month_frame) <- c(MONTHS[e])			       
  renderer$setData("column", month_frame)
  treeview$insertColumnWithAttributes(-1, as.character(MONTHS[e]), renderer, text = e, editable = 13)
}
}

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
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
recruitments.cell_edited <- function(cell, path.string, new.text, data) {
  
  #checkPtrType(data, "GtkListStore")
  recruitments.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("Recruitment Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("Recruitment Edited column:", column))
  iter <- recruitments.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  #	print(paste("indice i:", i))
  	#print(recruitments[[i]])
  	recruitments[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(recruitments[[i]][column+1])
  	recruitments.model$set(iter, column, recruitments[[i]][column+1])
}

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
# Function for the saving of the vector
# ------------------------------------------------------------------------------
#
save_file_recruitment_vector <- function(w) {
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
# create pproduction table 
recruitment_table <- data.frame(matrix(nrow=length(years), ncol=(length(MONTHS)+2) ))
colnames(recruitment_table) <- c("year","seed", MONTHS)
recruitment_table$year <- years
recruitment_table$seed[1] <- gtkEntryGetText(entry_OFFSPRING_seedvalue)
     
for (i in 1:length(recruitments)) {
  for (m in 3:14) {
      recruitment_table[as.character(recruitment_table$year) == as.character(recruitments[[i]]$year),m] <-  recruitments[[i]][m-1]
  }
} 

write.table(recruitment_table, dialog$getFilename(),  sep=";", na = "",row.names = FALSE)
dialog$destroy()
} else {
dialog$destroy()
}
}

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
recruitment.sw$SetUsize(100, 80)  
recruitment.sw$add(recruitments.treeview)
vboxSRrelationship$packStart(recruitment.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vboxSRrelationship, recruitment.sw, 4)
gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)
    
}
