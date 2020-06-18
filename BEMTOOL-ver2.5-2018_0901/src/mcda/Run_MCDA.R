# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


#wsp<-ws[1]
#load(wsp)

Run_MCDA <- function() {

SCENARIO_IDENTIFIER <<- "Scenario"
#source(paste(getwd(), "/src/mcda/Weights.R", sep=""))

weights=read.csv(paste(getwd(), "/src/mcda/Weights.csv", sep=""),sep=';',header=TRUE)  # Read the .csv file containing weights  

w <<- weights$Value
if(sum(w)!=1) w<<- w/sum(w) else w<<- w 
names(w)=weights$Name        # Check if weights sum to 1, otherwise normalize
k_GVA_ROI_PROFITS<<- as.numeric(as.character(w[1]))
k_RBER<<- as.numeric(as.character(w[2]))
k_WAGE<<- as.numeric(as.character(w[3]))
k_EMPL<<- as.numeric(as.character(w[4]))
k_SSB<<- as.numeric(as.character(w[5]))
k_F<<- as.numeric(as.character(w[6]))
k_Y<<- as.numeric(as.character(w[7]))
k_D<<- as.numeric(as.character(w[8]))
 


#source(paste(getwd(), "/src/mcda/Utility_parameters.R", sep=""))


utility_params=read.csv(paste(getwd(), "/src/mcda/Utility_params.csv", sep=""),sep=';',header=TRUE)  # Read the .csv file containing weights  

u_gva_mey<<- as.numeric(as.character(utility_params$Value[1]))
u_gva_0.5mey<<- as.numeric(as.character(utility_params$Value[2]))

#u_roi_mey<-as.numeric(as.character(utility_params$Value[3]))
#u_roi_0.5mey<-as.numeric(as.character(utility_params$Value[4]))

u_rber_1<<- as.numeric(as.character(utility_params$Value[3]))
u_rber_1.5<<- as.numeric(as.character(utility_params$Value[4]))

u_empl_ce<<- as.numeric(as.character(utility_params$Value[5]))
u_empl_0.5ce<<- as.numeric(as.character(utility_params$Value[6]))

u_wage_mnw<<- as.numeric(as.character(utility_params$Value[7]))

u_ssb_0.2<<- as.numeric(as.character(utility_params$Value[8]))
u_ssb_msy<<- as.numeric(as.character(utility_params$Value[9]))

u_f_msy<<- as.numeric(as.character(utility_params$Value[10]))
u_f_2msy<<- as.numeric(as.character(utility_params$Value[11]))

u_y_msy<<- as.numeric(as.character(utility_params$Value[12]))
u_y_0.5msy<<- as.numeric(as.character(utility_params$Value[13]))

u_d_0.25<<- as.numeric(as.character(utility_params$Value[14]))
u_d_0.5<<- as.numeric(as.character(utility_params$Value[15]))


GVA_or_ROI_or_PROFITS<<- as.character(utility_params$Value[16])
last_values<<- as.numeric(as.character(utility_params$Value[17]))


#source(paste(getwd(), "/src/mcda/Functions.R", sep=""))

# The following variables should be already available from the bmtconfig file: years, years.forecast, casestudy_path, length(BMT_SPECIES); otherwise, load_function.r should be run

years.all <- c(years,years.forecast) 
last_values <- ifelse(length(years.all)>last_values,last_values,(length(years.all)-1))

case_studies <- list.files(casestudy_path)  ## list all folders in the directory of case studies
# case_studies_HR <- case_studies[grep('HR',substr(case_studies,1,2))] ## list all folders related to HR scenarios
case_studies_HR <- case_studies[grep(SCENARIO_IDENTIFIER,substr(case_studies,1,nchar(SCENARIO_IDENTIFIER)))] ## list all folders related to HR scenarios
case_studies_path <- paste(casestudy_path,'/',case_studies_HR,sep='')

MEY_dir <- case_studies[grep('MEY',case_studies)]  ## select the folder containg MEY calculation

MEY_name <- list.files(paste(casestudy_path,'/',MEY_dir,sep=''),pattern = "\\.(csv|CSV)$")[grep('MEY.csv',list.files(paste(casestudy_path,'/',MEY_dir,sep=''),pattern = "\\.(csv|CSV)$"))]

MEY_path <- paste(casestudy_path,'/',MEY_dir,'/',MEY_name,sep='')

MEY <-  read.csv(MEY_path,sep=";",header=T,fileEncoding="Latin1")  

num_scenario <- length(case_studies_HR)

Summary_indicators <- matrix(0,nrow=num_scenario,ncol=8+(7*length(BMT_SPECIES)))

Unweighted_utilities <- matrix(0,nrow=8,ncol=num_scenario+1)

Unweighted_utilities[,1] <-w

MCDA <- matrix(0,nrow=8,ncol=num_scenario)

ut_names <-c(paste('u_',GVA_or_ROI_or_PROFITS,sep=''),'u_RBER','u_WAGE','u_EMPL','u_SSB','u_F','u_Y','u_D')
top_hierarchy <- c(rep('Socioeconomic',4),rep('Biological',4))
low_hierarchy <- c(rep('Economic',2),rep('Social',2),rep('Biol.Cons',2),rep('Biol.Prod',2))



for (i in (1:num_scenario)) {

name_bioind <- list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$")[grep('Biol',list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$"))]
if (length(grep('quantiles',name_bioind)) != 0) {
name_bioind <- name_bioind[name_bioind != name_bioind[grep('quantiles',name_bioind)] ]
}

name_pressind <- list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$")[grep('Press',list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$"))]
if (length(grep('quantiles',name_pressind)) != 0) {
name_pressind <- name_pressind[name_pressind != name_pressind[grep('quantiles',name_pressind)] ]
}


name_ecoind <- list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$")[grep('Eco',list.files(case_studies_path[i],pattern = "\\.(csv|CSV)$"))]
if (length(grep('quantiles',name_ecoind)) != 0) {
name_ecoind <- name_ecoind[name_ecoind != name_ecoind[grep('quantiles',name_ecoind)] ]
}

BIOIndicators <- read.csv(paste(case_studies_path[i],'/',name_bioind,sep=''), sep=";",header=T)

ECOindicators <- read.csv(paste(case_studies_path[i],'/',name_ecoind,sep=''), sep=";",header=T)

PRESSIMPindicators <-  read.csv(paste(case_studies_path[i],'/',name_pressind,sep=''), sep=";",header=T)


BIOIndicators_temp <- BIOIndicators[as.character(BIOIndicators$Year) != "ALL", ]

BIOIndicators_mcda <- BIOIndicators_temp[as.numeric(as.character(BIOIndicators_temp$Year))>=years.all[length(years.all)-last_values],]


ECOindicators_temp <- ECOindicators[ECOindicators$Fleet_segment=="ALL" & ECOindicators$Year!="ALL", ]

ECOindicators_mcda <- ECOindicators_temp[as.numeric(as.character(ECOindicators_temp$Year))>=years.all[length(years.all)-last_values],]

ECOindicators_mcda$Value <- as.numeric(as.character(ECOindicators_mcda$Value))

PRESSIMPindicators_temp <- PRESSIMPindicators[as.character(PRESSIMPindicators$Year) != "ALL" & as.character(PRESSIMPindicators$Fleet_segment) == "ALL", ]

PRESSIMPindicators_mcda <- PRESSIMPindicators_temp[as.numeric(as.character(PRESSIMPindicators_temp$Year))>=years.all[length(years.all)-last_values],]


#################################################################################
### Socioeconomic reference points values

# Minimum national wage 
    MNW <- unique(as.numeric(as.character(ECOindicators[ECOindicators$Variable=="Min.national.wage",]$Value)))

# Current employment if employment in the first year

    CE <- as.numeric(as.character(ECOindicators[ECOindicators$Year==years.all[1] & ECOindicators$Variable=="employment" & ECOindicators$Fleet_segment=="ALL",]$Value)) 


# From MEY file the following variables should be extracted:

    MEY_GVA <- MEY$Value[1]
    
    MEY_PROFITS <- MEY$Value[2]
    
    MEY_ROI <- MEY$Value[3]
    
    
### Socioeconomic indicator values (calculated as the mean over the last values of the series)

    GVA <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='gross.value.added',]$Value,na.rm=TRUE)

    ROI <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='Economic.indicators[ROI]',]$Value,na.rm=TRUE)

    PROFITS <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='profit',]$Value,na.rm=TRUE)

    RBER <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='total.revenues',]$Value/ECOindicators_mcda[ECOindicators_mcda$Variable=='Economic.indicators[break.even.revenue]',]$Value,na.rm=TRUE)

    WAGE <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='average.wage',]$Value,na.rm=TRUE)

    EMPL <- mean(ECOindicators_mcda[ECOindicators_mcda$Variable=='employment',]$Value,na.rm=TRUE)


#################################################################################
### Pressure reference points values

    FMSY <- PRESSIMPindicators[PRESSIMPindicators$Year=="ALL" & PRESSIMPindicators$Variable=='Fref',][order(PRESSIMPindicators[PRESSIMPindicators$Year=="ALL"&PRESSIMPindicators$Variable=='Fref',]$Stock),]$Value    #vector

    MSY <- PRESSIMPindicators[PRESSIMPindicators$Year=="ALL" & PRESSIMPindicators$Variable=='Yref',][order(PRESSIMPindicators[PRESSIMPindicators$Year=="ALL"&PRESSIMPindicators$Variable=='Yref',]$Stock),]$Value    #vector


### Pressure indicators values

    F <- tapply(PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='F',]$Value,PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='F' ,]$Stock,mean,na.rm=TRUE)
    
    #Y <- tapply(PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Y',]$Value,PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Y',]$Stock,mean,na.rm=TRUE)
    Y <- tapply(PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Catch',]$Value,PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Catch',]$Stock,mean,na.rm=TRUE)
    
#        D <- tapply(PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='D',]$Value,PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='D',]$Stock,mean,na.rm=TRUE)    
    D <- tapply(PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Discard',]$Value,PRESSIMPindicators_mcda[PRESSIMPindicators_mcda$Variable=='Discard',]$Stock,mean,na.rm=TRUE)


#################################################################################
### Biological reference points values

   SSBMSY <- BIOIndicators[BIOIndicators$Year=="ALL"&BIOIndicators$Variable=='SSBref',][order(BIOIndicators[BIOIndicators$Year=="ALL"&BIOIndicators$Variable=='SSBref',]$Stock),]$Value   # vector
    
    
    ### Biological indicator  values 
   
   SSB <- tapply(BIOIndicators_mcda[BIOIndicators_mcda$Variable=='SSB_exploited_pop',]$Value,BIOIndicators_mcda[BIOIndicators_mcda$Variable=='SSB_exploited_pop',]$Stock,mean,na.rm=TRUE)

   SSB0=tapply(BIOIndicators_mcda[BIOIndicators_mcda$Variable=='SSB_unexploited_pop',]$Value,BIOIndicators_mcda[BIOIndicators_mcda$Variable=='SSB_unexploited_pop',]$Stock,mean,na.rm=TRUE)



#################################################################################
### UTILITY_CALCULATION


U<- numeric(8) 


U[1]=ifelse(GVA_or_ROI_or_PROFITS=='GVA',UGVA(GVA,MEY_GVA,u_gva_mey,u_gva_0.5mey),ifelse(GVA_or_ROI_or_PROFITS=='ROI',UGVA(ROI,MEY_ROI,u_gva_mey,u_gva_0.5mey),UGVA(PROFITS,MEY_PROFITS,u_gva_mey,u_gva_0.5mey)))

U[2]<- URBER(RBER)
U[3]<- UWAGE(WAGE,MNW)
U[4]<- UEMPL(EMPL,CE)
U[5]<- prod(mapply(USSB,SSB,SSB0,SSBMSY))^(1/length(BMT_SPECIES))
U[6]<- prod(mapply(UF,F,FMSY))^(1/length(BMT_SPECIES))
U[7]<- UY(sum(Y),sum(MSY),u_y_msy,u_y_0.5msy)
U[8]<- UD(sum(D)/sum(Y))


Summary_indicators [i,]<- c(MNW,WAGE,CE,EMPL,ifelse(GVA_or_ROI_or_PROFITS=='GVA',GVA,ifelse(GVA_or_ROI_or_PROFITS=='PROFITS',PROFITS,ROI)),
ifelse(GVA_or_ROI_or_PROFITS=='GVA',MEY_GVA,ifelse(GVA_or_ROI_or_PROFITS=='PROFITS',MEY_PROFITS,MEY_ROI)),RBER,FMSY,F,MSY,Y,SSB,SSB0,SSBMSY,sum(D))

Unweighted_utilities[,i+1] <- round(U,4)

MCDA[,i] <- round(U*w,4)
}


colnames(Summary_indicators) <- c('MNW','WAGE','CE','EMPL',ifelse(GVA_or_ROI_or_PROFITS=='GVA','GVA',ifelse(GVA_or_ROI_or_PROFITS=='PROFITS','PROFITS','ROI')),
ifelse(GVA_or_ROI_or_PROFITS=='GVA','MEY_GVA',ifelse(GVA_or_ROI_or_PROFITS=='PROFITS','MEY_PROFITS','MEY_ROI')),'RBER',paste(rep('Fmsy',length(BMT_SPECIES)),names(F)),paste(rep('F',length(BMT_SPECIES)),names(F)),paste(rep('MSY',length(BMT_SPECIES)),names(F)),paste(rep('Catch',length(BMT_SPECIES)),names(F)),paste(rep('SSB',length(BMT_SPECIES)),names(F)),paste(rep('SSB0',length(BMT_SPECIES)),names(F)),paste(rep('SSBMSY',length(BMT_SPECIES)),names(F)),'D')
rownames(Summary_indicators)<- case_studies_HR

colnames(Unweighted_utilities) <- c('weights',case_studies_HR)
rownames(Unweighted_utilities) <-ut_names


colnames(MCDA) <- case_studies_HR
rownames(MCDA) <-ut_names

MCDA[is.na(MCDA[])] <- 0

### Save Results-Tables

save_path=paste(casestudy_path,'/','MCDA_results',sep='')
dir.create(save_path)

write.table(MCDA, file=paste(save_path,'/','MCDA.csv',sep=''), sep=";", row.names=TRUE,col.names=NA)

write.table(Unweighted_utilities, file=paste(save_path,'/','Unweighted_utilities.csv',sep=''), sep=";", row.names=TRUE,col.names=NA)

write.table(round(Summary_indicators,3), file=paste(save_path,'/','Summary_indicators.csv',sep=''), sep=";", row.names=TRUE,col.names=NA)


### Save Results-Graphs
### FIGURE 1
jpeg(file=paste(save_path,'/','MCDA_1.jpg',sep=''),width=21, height=21, bg="white", units="cm",res=200)  
par(mar=c(15, 12.1, 4.1, 5.1), xpd=TRUE)
mg <- barplot(apply(MCDA,2,sum, na.rm=T),col='grey',axes = FALSE, axisnames = FALSE,ylab='Utility', main="Overall utility")
text(mg, par('usr')[3], labels =colnames(MCDA), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)
 mtext( BMT_sw_version,side=4,outer=FALSE, cex = 0.7)

dev.off()

### FIGURE 2
jpeg(file=paste(save_path,'/','MCDA_2.jpg',sep=''),width=21, height=21, bg="white", units="cm",res=200) 
par(mar=c(15, 12.1, 4.1, 10.1), xpd=TRUE)

mg <- barplot(MCDA,col=topo.colors(8),axes = FALSE, axisnames = FALSE,ylab='Utility', main="Utility per indicator")
text(mg, par('usr')[3], labels =colnames(MCDA), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)
legend('topright',ut_names,fill=topo.colors(8),bty='n',ncol=1,inset=c(-0.4,0))
 mtext( BMT_sw_version,side=4,outer=FALSE, cex = 0.7)

dev.off()

### FIGURE 3
jpeg(file=paste(save_path,'/','MCDA_3.jpg',sep=''),width=21, height=21, bg="white", units="cm",res=200)  
par(mar=c(15, 12.1, 4.1, 10.1), xpd=TRUE)
barplot(mapply(rowsum,as.data.frame(MCDA),as.data.frame(low_hierarchy)),col=terrain.colors(4),ylab='Utility',main="Utility per group of indicators", axes = FALSE, axisnames = FALSE)
text(mg, par('usr')[3], labels =colnames(MCDA), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)
legend('topright',levels(as.factor(low_hierarchy)),fill=terrain.colors(4),bty='n',ncol=1,inset=c(-0.4,0))
 mtext( BMT_sw_version,side=4,outer=FALSE, cex = 0.7)

dev.off()


### FIGURE 4
jpeg(file=paste(save_path,'/','MCDA_4.jpg',sep=''),width=21, height=21, bg="white", units="cm",res=200)  
par(mar=c(15, 12.1, 4.1, 10.1), xpd=TRUE)
barplot(mapply(rowsum,as.data.frame(MCDA),as.data.frame(top_hierarchy)),col=topo.colors(2),ylab='Utility',main="Utility per macro group of indicators", axes = FALSE, axisnames = FALSE)
text(mg, par('usr')[3], labels =colnames(MCDA), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)
legend('topright',levels(as.factor(top_hierarchy)),fill=topo.colors(2),bty='n',ncol=1,inset=c(-0.4,0))
 mtext( BMT_sw_version,side=4,outer=FALSE, cex = 0.7)

dev.off()

}

