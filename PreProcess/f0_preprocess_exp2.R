#### EXPERIMENTO 2            
#### 1- Preprocesamiento de los datos 
##
## input: 
##       - archivos con los datos de cada sujeto. en carpeta data_raw/
##
## output:
##       - exp2_data.csv
##       - exp2_data.RData
##
## updated: Dec 5 2021
## 

rm(list=ls())

outputfileCSV   <- "data/exp2_data.csv"
outputfileR     <- "data/exp2_data.RData"
fun_preprocess2 <- dget("./PreProcess/fun_preprocess_exp2.R")
dexp2           <- fun_preprocess2()

write.csv(dexp2, outputfileCSV, row.names = FALSE)
save(dexp2,  file = outputfileR)
