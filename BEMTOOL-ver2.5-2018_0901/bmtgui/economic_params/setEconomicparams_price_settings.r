# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
setEconomicparams_price_settings <- function(w) {

bmt_fleet.price_elast_landing_byfleet <<- NULL
bmt_fleet.price_elast_import <<- NULL  
bmt_fleet.price_elast_MW <<- NULL 
bmt_fleet.price_importweight <<- NULL

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_price_models)

if (!is.null(selected) ) {
index_to_update <- which(PRICE_MODELS$option_name == selected)  

if (PRICE_MODELS$model_name[index_to_update] == 1) {
gtkLabelSetText(lblPriceFunction, "p_L[t]  =  p_L[t-1]  *  ( 1  +  a * ( L[t] - L[t-1] ) / L[t-1] ) ")

} else if (PRICE_MODELS$model_name[index_to_update] == 2) {
gtkLabelSetText(lblPriceFunction, "p[t]  =  p[t=0]  *  L[t] ^ a  *  imp[t] ^ b  *  meanW[t] ^ c ")

} else if (PRICE_MODELS$model_name[index_to_update] == 3) {
gtkLabelSetText(lblPriceFunction, "p[t]  =  p[t-1]  *  L[t] / L[t-1] ")

} else if (PRICE_MODELS$model_name[index_to_update] == 4) {
gtkLabelSetText(lblPriceFunction, "p[t]  =  p[t=0]  *  e ^ ( L[t] * a ) ")

} else if (PRICE_MODELS$model_name[index_to_update] == 5) {
gtkLabelSetText(lblPriceFunction, "p[t]  =  p[t-1] ")

} else if (PRICE_MODELS$model_name[index_to_update] == 6) {
gtkLabelSetText(lblPriceFunction, "p[t]  =  d ")

}
  
activate_deactivate_price_tables(PRICE_MODELS$model_name[index_to_update])
mat_cfg_price[2,2] <<- which(PRICE_MODELS$option_name == selected)
print(paste("Option selected for landing price calculation:", mat_cfg_price[2,2] ), quote=F)    

if (!is.na(mat_cfg_price[2,2])) {

#if (PRICE_MODELS$model_name[index_to_update] == mat_cfg_price[2,2]) {  
# ------------------------------------------------------------ ------------------------------------------------------------ 
if ( PRICE_MODELS$model_name[index_to_update] == 1 | PRICE_MODELS$model_name[index_to_update] == 4) {       # ------------------------------------------------------------ opzione 1 (ex-BIRDMOD)
  
matrix_temp <-   mat_cfg_price[4:(3+length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)), 1:5]
 price_elast_landing_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_landing_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_landing_byfleet_matrix$Species <- BMT_SPECIES
     
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (spee in 1:length(BMT_SPECIES)) {
       price_elast_landing_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,2] ))
}      
}

# involved input :
bmt_fleet.price_elast_landing_byfleet <<- price_elast_landing_byfleet_matrix

# ------------------------------------------------------------ ------------------------------------------------------------ 
} else if ( PRICE_MODELS$model_name[index_to_update] == 2 ) {    # ------------------------------------------------------------ opzione 2 (ex-MEFISTO)

matrix_temp <-   mat_cfg_price[4:(3+length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)), ]
 price_elast_landing_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_landing_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_landing_byfleet_matrix$Species <- BMT_SPECIES
 
 price_elast_import_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_import_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_import_byfleet_matrix$Species <- BMT_SPECIES 
     
     price_elast_WM_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_WM_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_WM_byfleet_matrix$Species <- BMT_SPECIES 
     
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (spee in 1:length(BMT_SPECIES)) {
       price_elast_landing_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,2] ))
       price_elast_import_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,3] ))
        price_elast_WM_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,4] ))
}      
}

matrix_temp <-   mat_cfg_price[(5+length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)):nrow(mat_cfg_price), ]
    price_import_weight_byyear_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(price_import_weight_byyear_matrix) <-  c("Species",BMT_YEARS_FORECAST)
     price_import_weight_byyear_matrix$Species <- BMT_SPECIES  


for (flee in 1:length(BMT_YEARS_FORECAST)) {
for (spee in 1:length(BMT_SPECIES)) {
       price_import_weight_byyear_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.import.S", spee, sep="") ,flee+1] ))
}      
}

# involved input :
     bmt_fleet.price_elast_landing_byfleet <<- price_elast_landing_byfleet_matrix
     bmt_fleet.price_elast_import <<- price_elast_import_byfleet_matrix
     bmt_fleet.price_elast_MW <<- price_elast_WM_byfleet_matrix
     bmt_fleet.price_importweight <<- price_import_weight_byyear_matrix

} 

}

  bmt_reload_price_elast_landing_byfleet_table()
  bmt_reload_price_elast_import_table()  
  bmt_reload_price_elast_MW_table()     
  bmt_reload_price_importweight_table()
   
}


} 