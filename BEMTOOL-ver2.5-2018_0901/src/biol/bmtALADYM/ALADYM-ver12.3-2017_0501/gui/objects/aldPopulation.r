# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ------------------------------------------------------------------------------
# Definition of object types used in BEMTOOL code (S4 classes and methods)
# - object Population used in ALADYM
# ------------------------------------------------------------------------------

# definition of object: aladymPopulation
setClass(Class="aldPopulation",
        representation=representation(
            GSA = "character",
            scientific_name = "character",
            common_name = "character",
            lengthweight = "data.frame",
            maturityogive = "data.frame",
            growth = "data.frame",
            lifespan = "data.frame",
            sexratio = "numeric"
            ) )


#
#
#
#
#           
setPopulationfromGUI<-function(object) {

# general parameters
object@GSA <- gtkEntryGetText(entryGSA)
object@scientific_name <- gtkEntryGetText(entrySpecies)
object@common_name <- gtkEntryGetText(entrySpeciesCommonName)

# set length-weight parameters
lengthweight_df <- data.frame(rbind(list(rowname="M", a = as.numeric(gtkEntryGetText(entryAB_A_M)), b=as.numeric(gtkEntryGetText(entryAB_B_M))), list(rowname= "F", a=as.numeric(gtkEntryGetText(entryAB_A_F)), b=as.numeric(gtkEntryGetText(entryAB_B_F)))), row.names=1)
object@lengthweight <- lengthweight_df

# set maturity ogive parameters
maturity_params_df <- data.frame(rbind(list(rowname="M.L50", 
                                            distribution=gtkComboBoxGetActiveText(combo_L50dis_M), 
                                            min=as.numeric(gtkEntryGetText(entryOGIVEL50_M_min)), 
                                            max=as.numeric(gtkEntryGetText(entryOGIVEL50_M_max)), 
                                            A=as.numeric(gtkEntryGetText(entryOGIVEL50_M_a)), 
                                            B=as.numeric(gtkEntryGetText(entryOGIVEL50_M_b)) ), 
                                       list(rowname="M.L75L25", 
                                            distribution=gtkComboBoxGetActiveText(combo_L75L25dis_M), 
                                            min=as.numeric(gtkEntryGetText(entryOGIVEL75L25_M_min)), 
                                            max=as.numeric(gtkEntryGetText(entryOGIVEL75L25_M_max)), 
                                            A=as.numeric(gtkEntryGetText(entryOGIVEL75L25_M_a)), 
                                            B=as.numeric(gtkEntryGetText(entryOGIVEL75L25_M_b)) ), 
                                       list(rowname="F.L50", 
                                            distribution=gtkComboBoxGetActiveText(combo_L50dis_F), 
                                            min=as.numeric(gtkEntryGetText(entryOGIVEL50_F_min)), 
                                            max=as.numeric(gtkEntryGetText(entryOGIVEL50_F_max)), 
                                            A=as.numeric(gtkEntryGetText(entryOGIVEL50_F_a)), 
                                            B=as.numeric(gtkEntryGetText(entryOGIVEL50_F_b)) ) ,
                                       list(rowname="F.L75L25", 
                                            distribution=gtkComboBoxGetActiveText(combo_L75L25dis_F), 
                                            min=as.numeric(gtkEntryGetText(entryOGIVEL75L25_F_min)), 
                                            max=as.numeric(gtkEntryGetText(entryOGIVEL75L25_F_max)), 
                                            A=as.numeric(gtkEntryGetText(entryOGIVEL75L25_F_a)), 
                                            B=as.numeric(gtkEntryGetText(entryOGIVEL75L25_F_b)) ) ), row.names=1)
object@maturityogive <- maturity_params_df    

# set growth function parameters
growth_params_df <- data.frame(rbind(list(rowname="M.t0",
                                            distribution=gtkComboBoxGetActiveText(combo_t0dis_M), 
                                            min=as.numeric(gtkEntryGetText(entryVBFtzero_M_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFtzero_M_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFt0_M_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFt0_M_b)) ), 
                                     list(rowname="M.K", 
                                            distribution=gtkComboBoxGetActiveText(combo_Kdis_M), 
                                            min=as.numeric(gtkEntryGetText(entryVBFK_M_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFK_M_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFK_M_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFK_M_b)) ), 
                                     list(rowname="M.Linf", 
                                            distribution=gtkComboBoxGetActiveText(combo_Linfdis_M), 
                                            min=as.numeric(gtkEntryGetText(entryVBFLinf_M_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFLinf_M_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFLinf_M_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFLinf_M_b)) ) ,
                                     list(rowname="F.t0",
                                            distribution=gtkComboBoxGetActiveText(combo_t0dis_F), 
                                            min=as.numeric(gtkEntryGetText(entryVBFtzero_F_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFtzero_F_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFt0_F_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFt0_F_b)) ), 
                                     list(rowname="F.K", 
                                            distribution=gtkComboBoxGetActiveText(combo_Kdis_F), 
                                            min=as.numeric(gtkEntryGetText(entryVBFK_F_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFK_F_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFK_F_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFK_F_b)) ), 
                                     list(rowname="F.Linf", 
                                            distribution=gtkComboBoxGetActiveText(combo_Linfdis_F), 
                                            min=as.numeric(gtkEntryGetText(entryVBFLinf_F_min)), 
                                            max=as.numeric(gtkEntryGetText(entryVBFLinf_F_max)), 
                                            A=as.numeric(gtkEntryGetText(entryVBFLinf_F_a)), 
                                            B=as.numeric(gtkEntryGetText(entryVBFLinf_F_b)) ) ), row.names=1)                                        
object@growth <- growth_params_df  

# creation of lifespan data frame
lifespan_df <- data.frame(rbind(list(rowname="M", lifespan = as.numeric(gtkEntryGetText(entryVBF_M_lifespan))), list(rowname= "F", lifespan = as.numeric(gtkEntryGetText(entryVBF_F_lifespan)))), row.names=1)
object@lifespan <- lifespan_df   

# sex ratio     
object@sexratio <- as.numeric(gtkEntryGetText(entry_SR_value) )                                         

return(object)
}
