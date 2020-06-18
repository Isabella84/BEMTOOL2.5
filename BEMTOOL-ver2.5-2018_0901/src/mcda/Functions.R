# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




UGVA=function(GVA,MEY,u_gva_mey,u_gva_0.5mey){
	if(u_gva_mey==2*u_gva_0.5mey){
		UGVA<-ifelse(GVA<=0,0,min(1,GVA/MEY))
		return(UGVA)
		}
else {
	b=-2/MEY*(log((u_gva_mey-u_gva_0.5mey)/u_gva_0.5mey))
	a=u_gva_mey/(1-exp(-b*MEY))
	UGVA<-ifelse(GVA<=0,0,min(1,a*(1-exp(-b*GVA))))
	return(UGVA)}
	}


URBER=function(RBER)
{b=(1.5*u_rber_1.5-0.5-u_rber_1)/(u_rber_1.5-u_rber_1);a=1-((1-b)*u_rber_1);ifelse(RBER<1,0,(RBER-a)/(RBER-b))}


UWAGE=function(WAGE,MNW)
{b=-log(u_wage_mnw)/MNW;ifelse(WAGE<MNW,0,(1-exp(-b*WAGE)))}	

UEMPL=function(EMPL,CE)
{b=(CE-0.5*CE)/(log(1/u_empl_ce-1)-log(1/u_empl_0.5ce-1));a=b*log(1/u_empl_0.5ce-1)-0.5*CE;1/(1+exp((EMPL+a)/b))}

USSB=function(SSB,SSB0,SSBMSY)
{b=(0.2*SSB0-SSBMSY)/(log(1/u_ssb_0.2-1)-log(1/u_ssb_msy-1));a=b*log(1/u_ssb_msy-1)-SSBMSY;1/(1+exp((SSB+a)/b))}

UF=function(F,FMSY)
{b=(FMSY)/(log(1/u_f_2msy-1)-log(1/u_f_msy-1));a=b*log(1/u_f_msy-1)-FMSY;1/(1+exp((F+a)/b))}



UY=function(Y,MSY,u_y_msy,u_y_0.5msy){
	if(u_y_msy==2*u_y_0.5msy){
		UY<-min(1,Y/MSY)
		return(UY)
		}
else {
	b=-2/MSY*(log((u_y_msy-u_y_0.5msy)/u_y_0.5msy))
	a=u_y_msy/(1-exp(-b*MSY))
	UY<-min(1,a*(1-exp(-b*Y)))
	return(UY)}
	}


UD=function(D)
{b=(0.25-0.5)/(log(1/u_d_0.25-1)-log(1/u_d_0.5-1));a=b*log(1/u_d_0.25-1)-0.25;1/(1+exp((D+a)/b))}

