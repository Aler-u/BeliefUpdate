fun_preprocess_exp1 <- function(file1, file2, file3) {
  
  require(dplyr)
  
  ## cargo los datos crudos (vienen de la planilla de gdoc "RAW DATA - PRE PROCESS"
  data1 <- read.csv(file1)   # contiene el indice de scoreAnsiedad, scoreOptimismo_r y otros de cada sujeto 
  data2 <- read.csv(file2)   # contiene la respuesta de cada participante en cada trial
  data3 <- read.csv(file3)   # contiene el valor verdadero de cada trial
  
  ## cambio los nombres de las columnas y elimino algunas por ahora (ci son columnas que descarto por ahora)
  colnames(data1) <- c("sujID", "scoreAnsiedad", "scoreOptimismo_r", "scoreOptimismo", "scoreIncertidumbre", "c1", "c2", "c3", "c4", "scorePes", "scoreOpt", "scoreLOT")
  colnames(data2) <- c("sujID", "stimID", "Igood", "error", "update", "memory1", "c1", "c2", "memory2", "c3", "c4", "scr1", "scr2") 
  colnames(data3) <- c("stimID", "c1", "c2", "c3", "pT" )
  
  data1 <- select(data1, -c(c1, c2, c3, c4))
  data2 <- select(data2, -c(c1, c2, c3, c4))
  data3 <- select(data3, -c(c1, c2, c3))
  
  ## int -> numeric y cambio codigo de valencia (1: good news; 0: bad news)
  data1$scoreAnsiedad       <- as.numeric( data1$scoreAnsiedad )
  data1$scoreOptimismo_r    <- as.numeric( data1$scoreOptimismo_r )
  data1$scoreOptimismo      <- as.numeric( data1$scoreOptimismo )
  data1$scorePes            <- as.numeric( data1$scorePes )
  data1$scoreOpt            <- as.numeric( data1$scoreOpt )
  data1$scoreLOT            <- as.numeric( data1$scoreLOT )
  data1$scoreIncertidumbre  <- as.numeric( data1$scoreIncertidumbre )
  data2$Igood               <- ifelse(data2$Igood==100, 1, 0)
  data3$pT                  <- as.numeric(data3$pT)
  
  ## junto todo en un unico dataframe
  
  # agrego columnas con datos psi
  sID <- unique(data2$sujID) 
  N   <- length(sID)         
  d   <- data2
  
  for (i in 1:N){
    for (j in 1:nrow(data2)){
      if (data2$sujID[j]==sID[i]){
        d$scoreAnsiedad[j]       <- data1$scoreAnsiedad[i]
        d$scoreIncertidumbre[j]  <- data1$scoreIncertidumbre[i]
        d$scoreOptimismo_r[j]    <- data1$scoreOptimismo_r[i]
        d$scoreOptimismo[j]      <- data1$scoreOptimismo[i]
        d$scorePes[j]    <- data1$scorePes[i]
        d$scoreOpt[j]    <- data1$scoreOpt[i]
        d$scoreLOT[j]    <- data1$scoreLOT[i]
      }
    }
  }
  
  # agrego la probabilidad real
  stimID <- unique(data3$stimID) 
  Nstim  <- length(stimID)          # length(sID)          
  
  for (j in 1:nrow(d)){
    for (i in 1:Nstim){    
      if( d$stimID[j] == stimID[i] ){
        d$pT[j]  <- data3$pT[i]
      }
    }
  }
  
  ## calculo p1, p2 (las respuestas antes y despues de cada sujeto)
  for (j in 1:nrow(d)){
    
    if (d$Igood[j]==1){
      d$p1[j] <- d$pT[j] + d$error[j]
      d$p2[j] <- d$pT[j] + d$error[j] - d$update[j]
    }
    if (d$Igood[j]==0){
      d$p1[j] <- d$pT[j] - d$error[j]
      d$p2[j] <- d$pT[j] - d$error[j] + d$update[j]
    }
    
  }
  
  ## hay 7 casos de p1<0 o p2<0 (errores de transcripcion de los datos?) -> los elimino
  for (i in 1:nrow(d)){
    d$confirmar[i] <- 0
    if (d$p1[i]<0 || d$p2[i]<0 || d$p1[i]>100 || d$p2[i]>100){
      d$confirmar[i] <- 1
    }
  }
  d <- subset(d, d$confirmar==0)
  
  ## % --> probabilidades
  d$p1      <- d$p1/100
  d$p2      <- d$p2/100
  d$pT      <- d$pT/100
  d$error   <- d$error/100
  d$update  <- d$update/100
  d$memory1 <- d$memory1/100
  d$memory2 <- d$memory2/100
  
  
  # agrego columna de ID sujeto ordenada
  unique_subj <- unique(d$sujID)
  sID         <- rep(0, nrow(d))
  for (i in 1:nrow(d)){
    for (j in 1:length(unique_subj)){
      if (d$sujID[i]==unique_subj[j]){
        sID[i] <- j
      }
    } 
  }
  d$subj <- sID
  
  
  # agrego columna de error de prediccion y de update definido como prefiero
  d$ep        <- d$p1-d$pT
  d$update    <- ifelse(d$ep>0, d$p1-d$p2,  d$p2-d$p1)
  d$ep        <- abs(d$ep)
  
  
  # estandarizo scores
  d$scoreAnsiedad_std      <- ( d$scoreAnsiedad      - mean(data1$scoreAnsiedad) )      / sd(data1$scoreAnsiedad)
  d$scoreOptimismo_r_std   <- ( d$scoreOptimismo_r   - mean(data1$scoreOptimismo_r) )   / sd(data1$scoreOptimismo_r)
  d$scoreOptimismo_std     <- ( d$scoreOptimismo     - mean(data1$scoreOptimismo) )     / sd(data1$scoreOptimismo)
  d$scorePes_std   <- ( d$scorePes   - mean(data1$scorePes) )   / sd(data1$scorePes)
  d$scoreOpt_std   <- ( d$scoreOpt   - mean(data1$scoreOpt) )   / sd(data1$scoreOpt)
  d$scoreLOT_std   <- ( d$scoreLOT   - mean(data1$scoreLOT) )   / sd(data1$scoreLOT)
  d$scoreIncertidumbre_std <- ( d$scoreIncertidumbre - mean(data1$scoreIncertidumbre) ) / sd(data1$scoreIncertidumbre)
  d$altaAnsiedad           <- ifelse(d$scoreAnsiedad  >= 27, 1, 0)
  d$altoOptimismo          <- ifelse(d$scoreOptimismo >= median(data1$scoreOptimismo), 1, 0)
  d$altaIncertidumbre      <- ifelse(d$scoreIncertidumbre >= median(data1$scoreIncertidumbre), 1, 0)
  d$ep_std                 <- ( d$ep - mean(d$ep) ) / sd(d$ep)
  d$pT_std                 <- ( d$pT - mean(d$pT) ) / sd(d$pT)
  d$p1_std                 <- ( d$p1 - mean(d$p1) ) / sd(d$p1)
  d$p2_std                 <- ( d$p2 - mean(d$p2) ) / sd(d$p2)
  
  # scores en tres: baja/media/alta 
  q.ansiedad      <- quantile(data1$scoreAnsiedad, probs = c(0.33, 0.66))
  q.optimismo     <- quantile(data1$scoreOptimismo, probs = c(0.33, 0.66))
  q.incertidumbre <- quantile(data1$scoreIncertidumbre, probs = c(0.33, 0.66))
  
  d$scoreAnsiedad3levels <- ifelse( d$scoreAnsiedad > q.ansiedad[2], 1, 
                                    ifelse( d$scoreAnsiedad > q.ansiedad[1], 0, -1))
  d$scoreOptimismo3levels <- ifelse( d$scoreOptimismo > q.optimismo[2], 1, 
                                     ifelse( d$scoreOptimismo > q.optimismo[1], 0, -1))
  d$scoreIncertidumbre3levels <- ifelse( d$scoreIncertidumbre > q.incertidumbre[2], 1, 
                                         ifelse( d$scoreIncertidumbre > q.incertidumbre[1], 0, -1))
  
  
  # reordeno y cambio nombres
  col_order   <- c("sujID", "subj", "stimID", "p1", "pT", "p2", "p1_std", "pT_std", "p2_std", "Igood", "ep", "ep_std", "update", "memory1", "memory2", "scr1", "scr2", "scoreAnsiedad", "scoreOptimismo_r", "scoreOptimismo", "scoreIncertidumbre", "scoreAnsiedad_std", "scoreOptimismo_r_std", "scoreOptimismo_std", "scoreIncertidumbre_std", "altaAnsiedad", "altoOptimismo", "altaIncertidumbre", "scoreAnsiedad3levels", "scoreOptimismo3levels", "scoreIncertidumbre3levels", "scorePes", "scoreOpt", "scoreLOT", "scorePes_std", "scoreOpt_std", "scoreLOT_std")
  d           <- d[,col_order]
  colnames(d) <- c("sujID", "subj", "pregID", "p1", "pT", "p2", "p1_std", "pT_std", "p2_std", "I",     "ep", "ep_std", "update", "memory1", "memory2", "scr1", "scr2", "scoreAnsiedad", "scoreOptimismo_r", "scoreOptimismo", "scoreIncertidumbre", "scoreAnsiedad_std", "scoreOptimismo_r_std", "scoreOptimismo_std", "scoreIncertidumbre_std", "altaAnsiedad", "altoOptimismo", "altaIncertidumbre", "scoreAnsiedad3levels", "scoreOptimismo3levels", "scoreIncertidumbre3levels", "scorePes", "scoreOpt", "scoreLOT", "scorePes_std", "scoreOpt_std", "scoreLOT_std")
  
  d$subj    <- factor(d$subj)
  d$pregID  <- factor(d$pregID)
  
  return(d)
}
