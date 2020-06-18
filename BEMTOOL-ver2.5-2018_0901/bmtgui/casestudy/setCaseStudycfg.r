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
setCSname <- function(w) {

string_name <- gtkEntryGetText(entry_CaseStudy_name)

if (string_name != "")  {
m <- regexec("[[:alnum:]+[:punct:]+[:blank:]+]+", string_name)
#object@fleetname <- as.character(regmatches(string_fleet, m))

      mat_cfg_general[mat_cfg_general[,1] == "casestudy.name",2] <<- as.character(regmatches(string_name, m))
      mat_cfg_general[is.na(mat_cfg_general[])] <<- ""
      ret <- T
 } else {
      showError("Set the name of the Case Study!")
      ret <- F
 }
 
return(ret)
}



assign_casestudypath <- function(w) {
      path <- choose.dir(default = "", caption = "")
      
      if ( !is.na(path) )  {
      mat_cfg_general[mat_cfg_general[,1] == "casestudy.name",3] <<-  path
        mat_cfg_general[is.na(mat_cfg_general[])] <<- ""
        
    # if (!is.na(path))  {
     gtkEntrySetText(lbl_casestudy, path) 
     } else {
         showError("Select the directory of Case Study!")
     }     
}


setPeriod <- function(w) {

ret <- T

st_y <- as.numeric(gtkEntryGetText(entry_casestudy_StartYear_simulation))
en_y <- as.numeric(gtkEntryGetText(entry_casestudy_EndYear_simulation))
en_y_fore <- as.numeric(gtkEntryGetText(entry_casestudy_EndYear_forecast)) 

    if ( !is.na(st_y) ) { 
    ret <- T
    if (!is.na(en_y) ) {
          ret <- T
           if (!is.na(en_y_fore) ) { 
                 ret <- T
                 if (en_y <= st_y ) {
                        showError("End year of simulation must be less than Start year of simulation!")
                    ret <- F
                 } else if (en_y_fore <= en_y ) {
                        showError("End year of forecast must be less than End year of simulation!")
                    ret <- F
                 }
           } else {
                 showError("End year of forecast is not a valid value!")
                ret <- F
           }
    } else {
           showError("End year of simulation is not a valid value!")
         ret <- FALSE
    }
    } else {
    showError("Start year of simulation is not a valid value!")
        ret <- FALSE
    } 

    if (ret) {
             mat_cfg_general[mat_cfg_general[,1] == "casestudy.startsimulation",2] <<- st_y
              mat_cfg_general[mat_cfg_general[,1] == "casestudy.endsimulation",2] <<- en_y
              
              mat_cfg_general_fore[mat_cfg_general_fore[,1] == "casestudy.startforecast",2] <<- en_y +1
              mat_cfg_general_fore[mat_cfg_general_fore[,1] == "casestudy.endforecast",2] <<- en_y_fore
              
             mat_cfg_general[is.na(mat_cfg_general[])] <<- ""
    }  
      
return(ret)
}




set_mat_cfg_SF <- function(mat_cfg_SF) {

if (nrow(interactions) != 0) {
mat_cfg_SF <- data.frame(matrix( nrow=nrow(mat_cfg_S), ncol=(nrow(mat_cfg_F)+1) ))
colnames(mat_cfg_SF) <- c("", 	paste("[F", c(1:nrow(mat_cfg_F)), "]", sep="") )
mat_cfg_SF[,1] <- paste("casestudy.S", c(1:length(BMT_SPECIES)), ".associatedFleetsegment", sep="")

for (nr in 1:nrow(mat_cfg_S)) {
    fleetss <- as.character(interactions$Fleet_Segment[interactions$Species == BMT_SPECIES[nr] ] )
    mat_cfg_SF[nr, which(BMT_FLEETSEGMENTS %in%  fleetss)+1 ] <-  BMT_FLEETSEGMENTS[which(BMT_FLEETSEGMENTS %in%  fleetss)]
}

 mat_cfg_SF[is.na(mat_cfg_SF[])] <- ""

mat_cfg_general[mat_cfg_general[,1] == "casestudy.stockno",2] <<-  length(BMT_SPECIES)
mat_cfg_general[mat_cfg_general[,1] == "casestudy.fleetsegmentno",2] <<-  length(BMT_FLEETSEGMENTS)
 mat_cfg_general[is.na(mat_cfg_general[])] <<- ""

} 

 

return(mat_cfg_SF)
}







saveBMTCFG<-function(w) {

max_ncol <- c()
sum_rows <- 0
 
     for (matt in BMTCFG_SUBTABLES_SIMULATION ) {
     if (matt != "mat_cfg_assessment_tools") {
         obj <- data.frame(get(matt))
         max_ncol <- c(max_ncol, ncol(obj))
         sum_rows <- sum_rows + nrow(obj)
         } else {
              for (sp in 1:length(BMT_SPECIES) ) {
          obj <- mat_cfg_assessment_tools[[sp]]
            max_ncol <- c(max_ncol, ncol(obj))
             sum_rows <- sum_rows + nrow(obj)
     }
         }
     }
     

                                                       # , nrow=sum_rows
FINAL_BMTCFG <- data.frame(matrix("", ncol= max(max_ncol), nrow=0), stringsAsFactors =F)
            colnames(FINAL_BMTCFG) <- paste("col_", c(1:ncol(FINAL_BMTCFG)), sep="")
            
       for (matt in 1:length(BMTCFG_SUBTABLES_SIMULATION) ) {
       
     if ( BMTCFG_SUBTABLES_SIMULATION[matt] != "mat_cfg_assessment_tools" ) {
              obj <- get(BMTCFG_SUBTABLES_SIMULATION[matt])    
              
#              BMTCFG_SUBTABLES <<- c("mat_cfg_general", "mat_cfg_F", "mat_cfg_S", "mat_cfg_SF", "mat_cfg_species_settings", "mat_cfg_assessment_tools", "mat_cfg_ALADYM_sim", "mat_cfg_general_fore",  "mat_cfg_EffortData", "mat_cfg_LandingData", "mat_cfg_REF_points", "mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress")

                                                                                                                                                                                                                                                                                                                               # commented on version 2.5.5 , "mat_cfg_EconomicIndicator_simu"
              if ( !(BMTCFG_SUBTABLES_SIMULATION[matt] %in% c("mat_cfg_general_fore",  "mat_cfg_EffortData", "mat_cfg_LandingData","mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress", "mat_cfg_EconomicIndicator")) ) {     
              obj <- rbind(colnames(obj), obj)
              }
              to_add <- data.frame(cbind(obj, matrix("", ncol=(ncol(FINAL_BMTCFG)-ncol(obj)), nrow=nrow(obj))) )
           colnames(to_add) <- paste("col_", c(1:ncol(FINAL_BMTCFG)), sep="")
               FINAL_BMTCFG <- rbind(FINAL_BMTCFG, to_add)     
         } else {
                for (sp in 1:length(BMT_SPECIES) ) {
                  obj <- data.frame(mat_cfg_assessment_tools[[sp]] , stringsAsFactors =F)
                  to_add <- data.frame(cbind(obj, matrix("", ncol=(ncol(FINAL_BMTCFG)-ncol(obj)), nrow=nrow(obj))) )
           colnames(to_add) <- paste("col_", c(1:ncol(FINAL_BMTCFG)), sep="")
               FINAL_BMTCFG <- rbind(FINAL_BMTCFG, to_add)
           } 
         }
    
     }

return(FINAL_BMTCFG)
}







save_bmtcfg_file<-function(w) {

dialog <- gtkFileChooserDialog("Enter a name for the CASE STUDY bmtcfg.csv file", BMTmain_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
save_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {

FINAL_BMTCFG <- saveBMTCFG()

     write.table(FINAL_BMTCFG, save_path, sep=";", row.names=F, col.names=F)

    wnd <- showMessageOK(paste("        BEMTOOL CONFIGURATION file landing saved!        "))

}

}
