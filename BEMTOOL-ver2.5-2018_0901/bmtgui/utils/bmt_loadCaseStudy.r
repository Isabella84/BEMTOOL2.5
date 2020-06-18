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
#



bmt_loadCaseStudy <- function(w) {

dialog <- gtkFileChooserDialog("BEMTOOL 2.0 - Choose the CASE STUDY bmtcfg.csv file", BMTmain_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
path <- dialog$getFilename()

#read.csv(path, sep=";") 

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {

#path <- "C:\\FACCHINI_MT\\_MAREA - SC11 LANDMED - BEMTOOL sw\\BEMTOOL app-ver1.4-2014\\BEMTOOL-ver2-2014\\bmtconfig.csv"
   LOADED_CASESTUDY <<- T
          interactions <<-  data.frame(matrix(nrow=0,ncol=2))
colnames(interactions) <- c("Species", "Fleet_Segment")

nostocks <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostocks) <- c("Stocks")

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <- c("Fleets")

EFFORT_NUMBER_list <<- list()
EFFORT_DAY_list <<- list()
EFFORT_GT_list <<- list()
EFFORT_KW_list <<- list()

LANDING_list_all <<- list() 

   loadCaseStudycfg(path)
     }    

}
