# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




vboxFishingMortality <- gtkVBox(homogeneous = FALSE, 5)

vboxFishingMortality$packStart(gtkLabel("Age range for F calculated"), expand = FALSE, fill = FALSE) 

tbl_agerange <- gtkTable(2,5,homogeneous = FALSE)
 tbl_agerange$SetRowSpacings(2)
 tbl_agerange$SetColSpacings(30)
 tbl_agerange$SetBorderWidth(5)

 i=0   # column 0
 j=0
 tbl_agerange$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
i = i+1   # column 2
tbl_agerange$Attach(gtkLabel("min"),i, i+1, j, j+1)
i = i+1   # column 3
entry_agerange_M_min <- gtkEntry()
gtkEntrySetWidthChars(entry_agerange_M_min, NUMERICAL_ENTRY_LENGTH)   
tbl_agerange$Attach(entry_agerange_M_min,i, i+1, j, j+1)
i = i+1  # column 4
tbl_agerange$Attach(gtkLabel("max"),i, i+1, j, j+1)
i = i+1  # column 5
entry_agerange_M_max <- gtkEntry()
gtkEntrySetWidthChars(entry_agerange_M_max, NUMERICAL_ENTRY_LENGTH)  
tbl_agerange$Attach(entry_agerange_M_max,i, i+1, j, j+1)

i=i+1   # column 0
tbl_agerange$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)   
i = i+1   # column 2
tbl_agerange$Attach(gtkLabel("min"),i, i+1, j, j+1)
 i = i+1   # column 3
 entry_agerange_F_min <- gtkEntry()
gtkEntrySetWidthChars(entry_agerange_F_min, NUMERICAL_ENTRY_LENGTH)
tbl_agerange$Attach(entry_agerange_F_min,i, i+1, j, j+1)
    i = i+1  # column 4
tbl_agerange$Attach(gtkLabel("max"),i, i+1, j, j+1)
     i = i+1  # column 5
entry_agerange_F_max <- gtkEntry()
gtkEntrySetWidthChars(entry_agerange_F_max, NUMERICAL_ENTRY_LENGTH) 
tbl_agerange$Attach(entry_agerange_F_max,i, i+1, j, j+1)



# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
if (IN_BEMTOOL) {
      SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),1])   
if (SAtool == "VIT") {
  min_fVIT <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),5]) )  
 gtkEntrySetText(entry_agerange_M_min, min_fVIT)
 gtkEntrySetText(entry_agerange_F_min, min_fVIT)
 
  max_fVIT <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".StockAssessmentTool", sep=""),6]) ) +1  
 gtkEntrySetText(entry_agerange_M_max, max_fVIT)
 gtkEntrySetText(entry_agerange_F_max, max_fVIT)
 
 
  } else if (SAtool == "XSA") {
    min_fXSA <- XSAinfo[[ALADYM_spe]]$results$result@range[6]
    max_fXSA <- XSAinfo[[ALADYM_spe]]$results$result@range[7] +1
    
  gtkEntrySetText(entry_agerange_M_min, min_fXSA)
 gtkEntrySetText(entry_agerange_F_min, min_fXSA)
 
  gtkEntrySetText(entry_agerange_M_max, max_fXSA)
 gtkEntrySetText(entry_agerange_F_max, max_fXSA)
 
 
  } else if (SAtool == "from Report") {
    # da sistemare
    min_f_report <- as.numeric(as.character(ReportINFO[[ALADYM_spe]]$results$age_rangeF$min ))
    max_f_report <- as.numeric(as.character(ReportINFO[[ALADYM_spe]]$results$age_rangeF$max )) +1
    
      gtkEntrySetText(entry_agerange_M_min, min_f_report)
 gtkEntrySetText(entry_agerange_F_min, min_f_report)
 
  gtkEntrySetText(entry_agerange_M_max, max_f_report)
 gtkEntrySetText(entry_agerange_F_max, max_f_report)
      
  }
}
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


vboxFishingMortality$packStart(tbl_agerange, padding = 0) 