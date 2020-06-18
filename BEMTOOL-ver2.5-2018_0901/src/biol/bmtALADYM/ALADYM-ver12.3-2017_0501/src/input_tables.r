# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




input_tables<-function(){
Input_table_tempM = BAS$MAge
Input_table_tempM = cbind(Input_table_tempM,BAS$MLength)
Input_table_tempM = cbind(Input_table_tempM,BAS$MWeight)
Input_table_tempM = cbind(Input_table_tempM,BAS$MM)
Input_tableM = cbind(Input_table_tempM,BAS$MMaturity)
colnames (Input_tableM) = c("Age_males","Length_males","Weight_males","Natural mortality_males","Maturity_males")
write.table(Input_tableM, MALEINPUT_table,row.names=FALSE, sep=";")

Input_table_tempF = BAS$FAge
Input_table_tempF = cbind(Input_table_tempF,BAS$FLength)
Input_table_tempF = cbind(Input_table_tempF,BAS$FWeight)
Input_table_tempF = cbind(Input_table_tempF,BAS$FM)
Input_tableF = cbind(Input_table_tempF,BAS$FMaturity)
colnames (Input_tableF) = c("Age_females","Length_females","Weight_females","Natural mortality_females","Maturity_females")
write.table(round(Input_tableF,3), FEMALEINPUT_table,row.names=FALSE, sep=";")
}
