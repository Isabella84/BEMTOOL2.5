# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


set_survivability_C_males<- function(w) {

if (gtkEntryGetText(entry_escape_survivability_males) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_escape_survivability_males) )) & (as.numeric(gtkEntryGetText(entry_escape_survivability_males) ) >= 0 & as.numeric(gtkEntryGetText(entry_escape_survivability_males) ) <= 1) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@escape.survivability.constant) == 0)) {
    FleetList_simulation[[select_index]]@escape.survivability.constant <<- data.frame(males=1, females=1)   
} 

 FleetList_simulation[[select_index]]@escape.survivability.datatype <<- "C"  
FleetList_simulation[[select_index]]@escape.survivability.constant$males <<- as.numeric(gtkEntryGetText(entry_escape_survivability_males) )



print("escape.survivability.constant$males changed!", quote=F)

} else {
 showError("Value of costant survivability for males must be numeric and between 0 and 1!")
}

}

}


set_survivability_C_males_fore<- function(w) {

if (gtkEntryGetText(entry_escape_survivability_males_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_escape_survivability_males_fore) )) & (as.numeric(gtkEntryGetText(entry_escape_survivability_males_fore) ) >= 0 & as.numeric(gtkEntryGetText(entry_escape_survivability_males_fore) ) <= 1) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@escape.survivability.constant) == 0)) {
    FleetList_forecast[[select_index]]@escape.survivability.constant <<- data.frame(males=1, females=1)   
} 

 FleetList_forecast[[select_index]]@escape.survivability.datatype <<- "C"  
FleetList_forecast[[select_index]]@escape.survivability.constant$males <<- as.numeric(gtkEntryGetText(entry_escape_survivability_males_fore) )



print("escape.survivability.constant$males changed!", quote=F)

} else {
 showError("Value of costant survivability for males in forecast must be numeric and between 0 and 1!")
}

}

}



# ---------------------------------------------------------------------------------------------------------------------------


set_survivability_C_females<- function(w) {

if (gtkEntryGetText(entry_escape_survivability_females) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_escape_survivability_females) )) & (as.numeric(gtkEntryGetText(entry_escape_survivability_females) ) >= 0 & as.numeric(gtkEntryGetText(entry_escape_survivability_females) ) <= 1) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@escape.survivability.constant) == 0)) {

    FleetList_simulation[[select_index]]@escape.survivability.constant <<- data.frame(males=1, females=1)   
} 

 FleetList_simulation[[select_index]]@escape.survivability.datatype <<- "C"
FleetList_simulation[[select_index]]@escape.survivability.constant$females <<- as.numeric(gtkEntryGetText(entry_escape_survivability_females) )

print("escape.survivability.constant$females changed!", quote=F)

} else {
 showError("Value of costant survivability for females must be numeric and between 0 and 1!")
}

}

}





set_survivability_C_females_fore<- function(w) {

if (gtkEntryGetText(entry_escape_survivability_females_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_escape_survivability_females_fore) )) & (as.numeric(gtkEntryGetText(entry_escape_survivability_females_fore) ) >= 0 & as.numeric(gtkEntryGetText(entry_escape_survivability_females_fore) ) <= 1) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@escape.survivability.constant) == 0)) {

    FleetList_forecast[[select_index]]@escape.survivability.constant <<- data.frame(males=1, females=1)   
} 

 FleetList_forecast[[select_index]]@escape.survivability.datatype <<- "C"
FleetList_forecast[[select_index]]@escape.survivability.constant$females <<- as.numeric(gtkEntryGetText(entry_escape_survivability_females_fore) )

print("escape.survivability.constant$females changed!", quote=F)

} else {
 showError("Value of costant survivability for females in forecast must be numeric and between 0 and 1!")
}

}

}







set_survivability_OGIVE_param1<- function(w) {

if (gtkEntryGetText(entry_survivability_ogive_param1) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_ogive_param1) )) & as.numeric(gtkEntryGetText(entry_survivability_ogive_param1) ) > 0 & as.numeric(gtkEntryGetText(entry_survivability_ogive_param1) ) < max(as.numeric(gtkEntryGetText(entryVBFLinf_F_max)), as.numeric(gtkEntryGetText(entryVBFLinf_M_max) ), na.rm=T )  ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams) == 0) ) {
    FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams <<- data.frame(param1=0, param2=0)   
} 

FleetList_simulation[[select_index]]@escape.survivability.DOS.datatype <<- "O"  
FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams$param1 <<- as.numeric(gtkEntryGetText(entry_survivability_ogive_param1) )


print("escape.survivability.DOS.ogiveparams$param1 changed!", quote=F)

} else {
 showError("Value of L50 of survivability reverse ogive must be numeric and between 0 and Linf!")
}

}

}




set_survivability_OGIVE_param1_fore<- function(w) {

if (gtkEntryGetText(entry_survivability_ogive_param1_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_ogive_param1_fore) )) & as.numeric(gtkEntryGetText(entry_survivability_ogive_param1_fore) ) > 0 & as.numeric(gtkEntryGetText(entry_survivability_ogive_param1_fore) ) < max(as.numeric(new_aldPopulation@growth[3,3]) , as.numeric(new_aldPopulation@growth[6,3]) , na.rm=T )  ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams) == 0) ) {
    FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams <<- data.frame(param1=0, param2=0)   
} 

FleetList_forecast[[select_index]]@escape.survivability.DOS.datatype <<- "O"  
FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams$param1 <<- as.numeric(gtkEntryGetText(entry_survivability_ogive_param1_fore) )


print("escape.survivability.DOS.ogiveparams$param1 changed!", quote=F)

} else {
 showError("Value of L50 of survivability reverse ogive in forecast must be numeric and between 0 and Linf!")
}

}

}






set_survivability_OGIVE_param2<- function(w) {

if (gtkEntryGetText(entry_survivability_ogive_param2) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_ogive_param2) )) & as.numeric(gtkEntryGetText(entry_survivability_ogive_param2) ) > 0 ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams) == 0) ) {
    FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams <<- data.frame(param1=0, param2=0)   
} 

 FleetList_simulation[[select_index]]@escape.survivability.DOS.datatype <<- "O"  
FleetList_simulation[[select_index]]@escape.survivability.DOS.ogiveparams$param2 <<- as.numeric(gtkEntryGetText(entry_survivability_ogive_param2) )



print("escape.survivability.DOS.ogiveparams$param2 changed!", quote=F)

} else {
 showError("Value of L75-L25 of survivability reverse ogive must be numeric and greater than 0!")
}

}

}






set_survivability_OGIVE_param2_fore<- function(w) {

if (gtkEntryGetText(entry_survivability_ogive_param2_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_ogive_param2_fore) )) & as.numeric(gtkEntryGetText(entry_survivability_ogive_param2_fore) ) > 0 ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams) == 0) ) {
    FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams <<- data.frame(param1=0, param2=0)   
} 

 FleetList_forecast[[select_index]]@escape.survivability.DOS.datatype <<- "O"  
FleetList_forecast[[select_index]]@escape.survivability.DOS.ogiveparams$param2 <<- as.numeric(gtkEntryGetText(entry_survivability_ogive_param2_fore) )



print("escape.survivability.DOS.ogiveparams$param2 changed!", quote=F)

} else {
 showError("Value of L75-L25 of survivability reverse ogive in forecast must be numeric and greater than 0!")
}

}

}











# DISCARD ***********************************************************************************


change_survivability_DISCARD_param1_males<- function(w) {

if (gtkEntryGetText(entry_survivability_param1_males) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_param1_males) )) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@discard.survivability.params) == 0)) {    #colnames(dat_fr) <- c("param1_or_M", "param2_or_F")
    FleetList_simulation[[select_index]]@discard.survivability.params <<- data.frame(param1_or_M=1, param2_or_F=1)   
} 

FleetList_simulation[[select_index]]@discard.survivability.params$param1_or_M <<- as.numeric(gtkEntryGetText(entry_survivability_param1_males) )

print("discard.survivability.params$males changed!", quote=F)

} else {
 showError("Value of discard survivability for males must be numeric!")
}

}

}


change_survivability_DISCARD_param1_males_fore<- function(w) {

if (gtkEntryGetText(entry_survivability_param1_males_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_param1_males_fore) )) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@discard.survivability.params) == 0)) {
    FleetList_forecast[[select_index]]@discard.survivability.params <<- data.frame(param1_or_M=1, param2_or_F=1)   
} 

FleetList_forecast[[select_index]]@discard.survivability.params$param1_or_M <<- as.numeric(gtkEntryGetText(entry_survivability_param1_males_fore) )

print("discard.survivability.params$males changed for FORECAST!", quote=F)

} else {
 showError("Value of discard survivability for males must be numeric!")
}

}

}




# ---------------------------------------------------------------------------------------------------------------------------


change_survivability_DISCARD_param2_females<- function(w) {

if (gtkEntryGetText(entry_survivability_param2_females) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_param2_females) )) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_simulation[[select_index]]@discard.survivability.params) == 0)) {
    FleetList_simulation[[select_index]]@discard.survivability.params <<- data.frame(param1_or_M=1, param2_or_F=1)    
} 

FleetList_simulation[[select_index]]@discard.survivability.params$param2_or_F <<- as.numeric(gtkEntryGetText(entry_survivability_param2_females) )

print("discard.survivability.params$females changed!", quote=F)

} else {
 showError("Value of costant survivability for females must be numeric!")
}

}

}



change_survivability_DISCARD_param2_females_fore<- function(w) {

if (gtkEntryGetText(entry_survivability_param2_females_fore) != "") {
if (!is.na(as.numeric(gtkEntryGetText(entry_survivability_param2_females_fore) )) ) {
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index <- which(FLEETSEGMENTS_names == selected)

if (all(dim(FleetList_forecast[[select_index]]@discard.survivability.params) == 0)) {
    FleetList_forecast[[select_index]]@discard.survivability.params <<- data.frame(param1_or_M=1, param2_or_F=1)    
} 

FleetList_forecast[[select_index]]@discard.survivability.params$param2_or_F <<- as.numeric(gtkEntryGetText(entry_survivability_param2_females_fore) )

print("discard.survivability.params$females for FORECAST changed!", quote=F)

} else {
 showError("Value of costant survivability for females must be numeric!")
}

}

}
