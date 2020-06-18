# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy,
# completeness or appropriateness for any particular purpose.



# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
setEconomicparams_price_settings_discard <- function(w) {

price_elast_discard_byfleet_MATRIX <<- NULL
price_costant_byfleet_discard_MATRIX <<- NULL


index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_price_models_discard)

if (!is.null(selected) ) {
index_to_update <- which(PRICE_MODELS_DISCARD$option_name == selected)

if (PRICE_MODELS_DISCARD$model_name[index_to_update] == 1) {
gtkLabelSetText(lblPriceFunction_discard, "p_D[t]  =  p_D[t-1]  *  ( 1  +  d * ( D[t] - D[t-1] ) / D[t-1] ) ")

} else if (PRICE_MODELS_DISCARD$model_name[index_to_update] == 2) {
gtkLabelSetText(lblPriceFunction_discard, "p_D[t]  =  p_D[t-1] ")

} else if (PRICE_MODELS_DISCARD$model_name[index_to_update] == 3) {
gtkLabelSetText(lblPriceFunction_discard, "p_D[t]  =  b ")

}

activate_deactivate_price_tables_discard(PRICE_MODELS_DISCARD$model_name[index_to_update])
mat_cfg_price[2,3] <<- which(PRICE_MODELS_DISCARD$option_name == selected)
print(paste("Option selected for discard price calculation:", mat_cfg_price[2,3] ), quote=F)

if (!is.na(mat_cfg_price[2,3])) {

#if (PRICE_MODELS$model_name[index_to_update] == mat_cfg_price[2,2]) {
# ------------------------------------------------------------ ------------------------------------------------------------
if ( PRICE_MODELS_DISCARD$model_name[index_to_update] == 1 ) {       # ------------------------------------------------------------ opzione 1 (ex-BIRDMOD)

matrix_temp <-   mat_cfg_price[4:(3+length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)), 1:5]

 price_elast_discard_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_discard_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_discard_byfleet_matrix$Species <- BMT_SPECIES

for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (spee in 1:length(BMT_SPECIES)) {
       price_elast_discard_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,5] ))
}
}

# involved input :
price_elast_discard_byfleet_MATRIX <<- price_elast_discard_byfleet_matrix

# ------------------------------------------------------------ ------------------------------------------------------------
} else if ( PRICE_MODELS_DISCARD$model_name[index_to_update] == 3 ) {       # ------------------------------------------------------------ opzione 1 (ex-BIRDMOD)

matrix_temp <-   mat_cfg_price[4:(3+length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)), 1:5]

 price_costant_discard_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_costant_discard_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_costant_discard_byfleet_matrix$Species <- BMT_SPECIES

for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (spee in 1:length(BMT_SPECIES)) {
       price_costant_discard_byfleet_matrix[spee, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.priceFun.F", flee,".S", spee, sep="") ,5] ))
}
}

# involved input :
price_costant_byfleet_discard_MATRIX <<- price_costant_discard_byfleet_matrix

# ------------------------------------------------------------ ------------------------------------------------------------
}

}

  reload_price_elast_discard_byfleet()
  reload_price_costant_byfleet_discard()

}


}