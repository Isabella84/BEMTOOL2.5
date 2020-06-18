# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





readSURBA<-function(m_spe, SURBAPath, n_ages) {

if (FALSE) {    
m_spe <-  m_int
SURBAPath <- filePath   
}

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", m_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

n_fleets <- length(associated_fleetsegment_indices)

SURBAMatrix <- read.csv(SURBAPath, sep=";", header=FALSE)

SURBA_age_classes <- as.numeric(as.character(SURBAMatrix[as.character(SURBAMatrix[,1]) == "Number" &  as.character(SURBAMatrix[,3]) == "ages", 4]))
SURBA_no_years <- as.character(SURBAMatrix[as.character(SURBAMatrix[,1]) == "Number" &  as.character(SURBAMatrix[,3]) == "years", 5])

start_y <- as.numeric(substring(SURBA_no_years, 2, 5))
end_y <- as.numeric(substring(SURBA_no_years, 7, 10))

years_period <-  end_y - start_y + 1

no_years_for_average <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),2] ) )

no_years_for_average <- ifelse(no_years_for_average > years_period, years_period, no_years_for_average)

start_this <- which(SURBAMatrix[,1] == "Natural" & SURBAMatrix[,2] == "mortality" )
matrix_this <- SURBAMatrix[start_this:(start_this+years_period+3), ]
matrix_this <- data.frame(matrix_this[5:nrow(matrix_this),(1:(SURBA_age_classes+1))]) 
colnames(matrix_this) <- c("year", paste("age", c(0:(SURBA_age_classes-1)), sep=""))

NaturalMortality <- data.frame(matrix_this[matrix_this$year %in% years,])

start_this <- which(SURBAMatrix[,1] == "Proportion" & SURBAMatrix[,2] == "mature" )
matrix_this <- SURBAMatrix[start_this:(start_this+years_period+3), ]
matrix_this <- data.frame(matrix_this[5:nrow(matrix_this),(1:(SURBA_age_classes+1))]) 
colnames(matrix_this) <- c("year", paste("age", c(0:(SURBA_age_classes-1)), sep=""))

Maturity <- data.frame(matrix_this[matrix_this$year %in% years,])

start_this <- which(SURBAMatrix[,1] == "Empirical" & SURBAMatrix[,2] == "stock" )
matrix_this <- SURBAMatrix[start_this:(start_this+years_period+3), ]
matrix_this <- data.frame(matrix_this[5:nrow(matrix_this),(1:5)]) 
colnames(matrix_this) <- c("year", "SSB", "Z", "SSB", "Z")

TotalMortality <- data.frame(matrix_this[matrix_this$year %in% years,])

TotalMortality[,ncol(TotalMortality)] <- as.numeric(as.character(TotalMortality[,ncol(TotalMortality)]) )

 TotalMortality[nrow(TotalMortality), ncol(TotalMortality)] <- mean(as.numeric(as.character(TotalMortality[(nrow(TotalMortality)-1):(nrow(TotalMortality)-no_years_for_average),ncol(TotalMortality)])))

start_this <- which(SURBAMatrix[,1] == "Recruitment" & SURBAMatrix[,2] =="(Jan" & SURBAMatrix[,6] == "1")
matrix_this <- SURBAMatrix[start_this:(start_this+years_period+3), ]
matrix_this <- data.frame(matrix_this[5:nrow(matrix_this),(1:7)]) 
colnames(matrix_this) <- c("year", "Estimate","5_percent", "25_percent", "50_percent", "75_percent", "95_percent")
Recruitment <- data.frame(matrix_this[matrix_this$year %in% years,])

Recruitment[,5] <- as.numeric(as.character(Recruitment[, 5]))
Recruitment[nrow(Recruitment), 5] <- mean(as.numeric(as.character(Recruitment[(nrow(Recruitment)-1):(nrow(Recruitment)-no_years_for_average),5])))

SURBAout <- list( age_classes=c(0:(SURBA_age_classes-1)), total_mortality=TotalMortality, natural_mortality=NaturalMortality, maturity=Maturity, recruitment=Recruitment) 

return(SURBAout)
}