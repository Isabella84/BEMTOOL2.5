# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





readReport<-function(m_spe, ReportPath, n_ages) {

if (FALSE) {     
m_spe <-  m_int
ReportPath <- filePath   
n_ages <- ages_C
}

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", m_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

n_fleets <- length(associated_fleetsegment_indices)

ReportMatrix <- data.frame(read.csv(ReportPath, sep=";", header=FALSE))

# Report_age_classes <- c(0:(n_ages-1))
                       # li leggo dalla cfg table
# set cursor = 1 (BEGIN)
cur <- 2

time_series_years <- c()

for (len_rep in 1:length(ReportMatrix[cur,])) {
     if ( !is.na(as.numeric(as.character(ReportMatrix[cur,len_rep]) )) ) {
         time_series_years <- c(time_series_years, as.numeric(as.character(ReportMatrix[cur,len_rep]) ))
     }
}

time_series_length <- length(time_series_years)

heading <-  t(ReportMatrix[cur, 1:time_series_length])
Report_F <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)] ) 
colnames(Report_F) <- heading
Report_age_classes <- as.numeric(as.character(Report_F[,1]))
Report_F <- Report_F[,2:ncol(Report_F)]
rownames(Report_F) <- paste("age", Report_age_classes, sep="")

# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_M <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_M) <- heading
Report_M <- data.frame(Report_M[,2:ncol(Report_M)])
rownames(Report_M) <- paste("age", Report_age_classes, sep="")
#print(VIT.catches_nb)

# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_maturity <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_maturity) <- heading
Report_maturity <- data.frame(Report_maturity[,2:ncol(Report_maturity)])
rownames(Report_maturity) <- paste("age", Report_age_classes, sep="")
#print(VIT.catches_w)


# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_stock_n <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_stock_n) <- heading
Report_stock_n <- data.frame(Report_stock_n[,2:ncol(Report_stock_n)])
rownames(Report_stock_n) <- paste("age", Report_age_classes, sep="")
#print(VIT.VPA_results_nb)


# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_stock_wt <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_stock_wt) <- heading
Report_stock_wt <- data.frame(Report_stock_wt[,2:ncol(Report_stock_wt)])
rownames(Report_stock_wt) <- paste("age", Report_age_classes, sep="")
#print(VIT.VPA_results_w)

# -----------------------------------------------------------------
# CATCHES
# -----------------------------------------------------------------

# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_catch_n <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_catch_n) <- heading
Report_catch_n <- data.frame(Report_catch_n[,2:ncol(Report_catch_n)])
rownames(Report_catch_n) <- paste("age", Report_age_classes, sep="")
#print(VIT.VPA_results_mortalities)


# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_catch_wt <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_catch_wt) <- heading
Report_catch_wt <- data.frame(Report_catch_wt[,2:ncol(Report_catch_wt)])
rownames(Report_catch_wt) <- paste("age", Report_age_classes, sep="")
#print(VIT.critical_length)

cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:((time_series_length*n_fleets)+1)])
Report_catch_by_fleet <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:((time_series_length*n_fleets)+1)]  )
colnames(Report_catch_by_fleet) <- heading
Report_catch_by_fleet <- data.frame(Report_catch_by_fleet[,2:ncol(Report_catch_by_fleet)])
rownames(Report_catch_by_fleet) <- paste("age", Report_age_classes, sep="")
#print(VIT.others)


# -----------------------------------------------------------------
# LANDINGS
# -----------------------------------------------------------------

# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_landing_n <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_landing_n) <- heading
Report_landing_n <- data.frame(Report_landing_n[,2:ncol(Report_landing_n)])
rownames(Report_landing_n) <- paste("age", Report_age_classes, sep="")
#print(VIT.VPA_results_mortalities)


# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_landing_wt <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_landing_wt) <- heading
Report_landing_wt <- data.frame(Report_landing_wt[,2:ncol(Report_landing_wt)])
rownames(Report_landing_wt) <- paste("age", Report_age_classes, sep="")
#print(VIT.critical_length)

cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:((time_series_length*n_fleets)+1)])
Report_landing_by_fleet <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:((time_series_length*n_fleets)+1)]  )
colnames(Report_landing_by_fleet) <- heading
Report_landing_by_fleet <- data.frame(Report_landing_by_fleet[,2:ncol(Report_landing_by_fleet)])
rownames(Report_landing_by_fleet) <- paste("age", Report_age_classes, sep="")
#print(VIT.others)

# -----------------------------------------------------------------
# DISCARDS
# -----------------------------------------------------------------

# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_discard_n <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_discard_n) <- heading
Report_discard_n <- data.frame(Report_discard_n[,2:ncol(Report_discard_n)])
rownames(Report_discard_n) <- paste("age", Report_age_classes, sep="")
#print(VIT.VPA_results_mortalities)


# update cursor to next table
cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:(time_series_length+1)])
Report_discard_wt <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:(time_series_length+1)]  )
colnames(Report_discard_wt) <- heading
Report_discard_wt <- data.frame(Report_discard_wt[,2:ncol(Report_discard_wt)] )
rownames(Report_discard_wt) <- paste("age", Report_age_classes, sep="")
#print(VIT.critical_length)

cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:((time_series_length*n_fleets)+1)])
Report_discard_by_fleet <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:((time_series_length*n_fleets)+1)]  )
colnames(Report_discard_by_fleet) <- heading
Report_discard_by_fleet <- data.frame(Report_discard_by_fleet[,2:ncol(Report_discard_by_fleet)])
rownames(Report_discard_by_fleet) <- paste("age", Report_age_classes, sep="")
#print(VIT.others)

cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:((time_series_length*n_fleets)+1)])
Report_F_by_fleet <- data.frame(ReportMatrix[c((cur+1):(cur+n_ages)), 1:((time_series_length*n_fleets)+1)]  )
colnames(Report_F_by_fleet) <- heading
Report_F_by_fleet <- data.frame(Report_F_by_fleet[,2:ncol(Report_F_by_fleet)])
rownames(Report_F_by_fleet) <- paste("age", Report_age_classes, sep="")
#print(VIT.others_2)

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

cur <- cur +  (n_ages+2)

heading <- t( ReportMatrix[cur, 1:6])
Report_ReferencePoints <- data.frame(ReportMatrix[c((cur+1):(cur+5)), 1:6]  )
colnames(Report_ReferencePoints) <- heading
RP_names <- Report_ReferencePoints[,1]
Report_ReferencePoints <- data.frame(Report_ReferencePoints[,2:ncol(Report_ReferencePoints)])
rownames(Report_ReferencePoints) <- RP_names
#print(VIT.reference_points)

cur <- cur +  (5+2)

heading <- t( ReportMatrix[cur, 1:2])
Agebar <- data.frame(ReportMatrix[cur+1, 1:2]  )
colnames(Agebar) <- heading

Reportout <- list( age_classes= Report_age_classes, timeseries=time_series_years, fishing_mortality=Report_F,  natural_mortality=Report_M, maturity=Report_maturity, stock_nb = Report_stock_n, stock_wt=Report_stock_wt, catches_nb=Report_catch_n,  catches_wt=Report_catch_wt, catches_by_fleet=Report_catch_by_fleet, landings_nb=Report_landing_n,  landings_wt=Report_landing_wt, landings_by_fleet=Report_landing_by_fleet, discards_nb=Report_discard_n,  discards_wt=Report_discard_wt, discards_by_fleet=Report_discard_by_fleet, F_by_fleet=Report_F_by_fleet, reference_points=Report_ReferencePoints, age_rangeF = Agebar) 

return(Reportout)
}
