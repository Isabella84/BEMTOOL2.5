# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


bmt_applyAutomaticReduction <- function(w) {

# bmt_resetAutomaticReduction(w)

percentage_reduction_r4 <- as.numeric(as.character(gtkEntryGetText(bmt_entry_reduction_r4) ))
timespan_reduction_r4 <- as.numeric(as.character( gtkEntryGetText(bmt_entry_timespan_r4) ))

if (is.na(percentage_reduction_r4) ) {
    showError("Percentage of reduction must be a numeric value!")
} else if (!(percentage_reduction_r4 <= 100 & percentage_reduction_r4 >= 0) ) {
    showError("Percentage of reduction must be a number between 0 and 100!")
} else if (is.na(timespan_reduction_r4) ) {
    showError("Time span must be a numeric value!")
} else if (!(timespan_reduction_r4 >= 1 & timespan_reduction_r4 <= length(BMT_YEARS_FORECAST) ) ) {
    showError("Time span must be at least 1 and not grater than the forecast period!")
} else {

selected_fleet <- gtkComboBoxGetActive(bmt_combo_fleetsegments_effort_r4)
if ( gtkNotebookGetCurrentPage(BMTnotebook_effort_r4) == 0) {  # 0 vessels,
 bmt_fleet.NUMBER_r4 <<-  EFFORT_NUMBER_list_fore[[selected_fleet+1]] 
    matrix_to_reduce <- EFFORT_NUMBER_list_fore[[selected_fleet+1]]
} else {      #   1 days
   bmt_fleet.DAY_r4 <<-   EFFORT_DAY_list_fore[[selected_fleet+1]] 
    matrix_to_reduce <- EFFORT_DAY_list_fore[[selected_fleet+1]]   
}  

print(paste("reduction of", percentage_reduction_r4, "in", timespan_reduction_r4, "years") )

print("Status quo:")
print(matrix_to_reduce)

new_reduced_matrix <- matrix_to_reduce

new_reduced_matrix <- data.frame( rbind(new_reduced_matrix, c("absolute reductions", rep(0, 12)) ) , stringsAsFactors=F)
#write.table(new_reduced_matrix, "C://test_automatic_reduction.csv", sep=";", row.names=F)                                                                                                    
  # B13-B13*(1-$B$29/100)
new_reduced_matrix[nrow(new_reduced_matrix), 2:13] <- as.numeric(as.character(new_reduced_matrix[1, 2:13])) - as.numeric(as.character(new_reduced_matrix[1, 2:13])) * (1 - percentage_reduction_r4 / 100)
 
   for (yer in 1:length(BMT_YEARS_FORECAST)) {
       yer_prev <- ifelse(yer==1,1,(yer-1)) 
#       print(BMT_YEARS_FORECAST[yer]-BMT_YEARS_FORECAST[1])
       if ( (BMT_YEARS_FORECAST[yer]-BMT_YEARS_FORECAST[1]) < timespan_reduction_r4 ) {
       #  =SE((B$35-$B$35)<$B$30;B13-$L39/$B$30;A39)
    new_reduced_matrix[yer,2:13]  <- as.numeric(as.character(new_reduced_matrix[yer_prev, 2:13])) - as.numeric(as.character(new_reduced_matrix[nrow(new_reduced_matrix),2:13])) / timespan_reduction_r4 
      
      } else {
     new_reduced_matrix[yer,2:13]  <-  as.numeric(as.character(new_reduced_matrix[yer_prev,2:13]))   
       }
      print(new_reduced_matrix[yer,2])   
        # da finireeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee  
      
   }      
# write.table(new_reduced_matrix, "C://test_automatic_reduction_final.csv", sep=";", row.names=F)                                                                                                    

#print("Reduced:")
#print(new_reduced_matrix)

if ( gtkNotebookGetCurrentPage(BMTnotebook_effort_r4) == 0) {  # 0 vessels,
   EFFORT_NUMBER_list_fore[[selected_fleet+1]]  <<- new_reduced_matrix[-nrow(new_reduced_matrix),]
   print( EFFORT_NUMBER_list_fore[[selected_fleet+1]])
     bmt_fleet.NUMBER_r4 <<-  EFFORT_NUMBER_list_fore[[selected_fleet+1]]  
   bmt_reload_NUMBER_r4_table()
} else {      #   1 days
    EFFORT_DAY_list_fore[[selected_fleet+1]] <<- new_reduced_matrix[-nrow(new_reduced_matrix),]  
    print(EFFORT_DAY_list_fore[[selected_fleet+1]])    
       bmt_fleet.DAY_r4 <<-   EFFORT_DAY_list_fore[[selected_fleet+1]] 
       bmt_reload_DAY_r4_table()
}  


}
                                  



}










bmt_resetAutomaticReduction <- function(w) {

selected_fleet <- gtkComboBoxGetActive(bmt_combo_fleetsegments_effort_r4)
print(paste("selezionato:", selected_fleet))

if ( gtkNotebookGetCurrentPage(BMTnotebook_effort_r4) == 0) {  # 0 vessels,

NUMBER_r4_matrix_temp <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(NUMBER_r4_matrix_temp) <- c("year",MONTHS)
     NUMBER_r4_matrix_temp$year <- BMT_YEARS_FORECAST  

    EFFORT_NUMBER_list_fore[[selected_fleet+1]] <<- NUMBER_r4_matrix_temp
     bmt_fleet.NUMBER_r4 <<- NULL 
   bmt_reload_NUMBER_r4_table()
} else {      #   1 days

 DAY_r4_matrix_temp <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(DAY_r4_matrix_temp) <- c("year",MONTHS)
     DAY_r4_matrix_temp$year <- BMT_YEARS_FORECAST  

    EFFORT_DAY_list_fore[[selected_fleet+1]] <<- DAY_r4_matrix_temp
    bmt_fleet.DAY_r4 <<-   NULL
       bmt_reload_DAY_r4_table()  
}  


}