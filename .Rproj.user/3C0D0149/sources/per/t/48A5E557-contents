#### EXPERIMENTO 1            
#### 1- Preprocesamiento de los datos 
##
## input: 
##       - data_psi.csv, data_suj.csv, pT.csv
##       - todos estos están en una planilla de gdrive "RAW DATA - PRE PROCESS"
##
## output:
##       - exp1_data.csv
##       - exp1_data.RData
##
## updated: Dec 5 2021
## 

rm(list=ls())

file1           <- "./data_raw/exp1/psychscores.csv"  # contiene el indice de scoreAnsiedad, scoreOptimismo_r y otros de cada sujeto 
file2           <- "./data_raw/exp1/trialdata.csv"  # contiene la respuesta de cada participante en cada trial
file3           <- "./data_raw/exp1/Ptrue.csv"        # contiene el valor verdadero de cada trial
outputfileCSV   <- "./data/exp1_data.csv"
outputfileR     <- "./data/exp1_data.RData"
fun_preprocess1 <- dget("./PreProcess/fun_preprocess_exp1.R")
dexp1           <- fun_preprocess1(file1, file2, file3)

write.csv(dexp1, outputfileCSV, row.names = FALSE)
save(dexp1,  file = outputfileR)
