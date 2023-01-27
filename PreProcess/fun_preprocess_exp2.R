fun_preprocess_exp2 <- function() {
  
  require(plyr)
  require(dplyr)
  
  ## read all csv and write a single file
  #mydir = "./data_raw/exp2"
  myfiles = list.files(path="./data_raw/exp2", pattern="*.csv", full.names=TRUE)
  myfiles
  dat_csv = plyr::ldply(myfiles, read.csv)
  
  #### CALCULO LOS PUNTAJES
  Nsuj <- nrow(dat_csv)
  LOTR <- data.frame(subj = 1:Nsuj, opt = rep(NA, Nsuj), pes = rep(NA, Nsuj), tot = rep(NA, Nsuj))
  STAI <- data.frame(subj = 1:Nsuj, pos = rep(NA, Nsuj), neg = rep(NA, Nsuj), tot = rep(NA, Nsuj))
  SHAI <- data.frame(subj = 1:Nsuj, pos = rep(NA, Nsuj), neg = rep(NA, Nsuj), tot = rep(NA, Nsuj))
  STAIestado <- data.frame(subj = 1:Nsuj, tot = rep(NA, Nsuj))
  
  #### LOT-R
  # Puntuar optimismo = 1,4, 10
  # Puntuar pesimismo (reversos)= 3, 7, 9 *
  # Puntaje total (sumando los reversos) = 1, 3, 4, 7, 9, 10
  
  LOTR$opt <- dat_csv$LOT1 + dat_csv$LOT4 + dat_csv$LOT10
  LOTR$pes <- (6-dat_csv$LOT3) + (6-dat_csv$LOT7) + (6-dat_csv$LOT9)
  LOTR$tot <- LOTR$opt + LOTR$pes
  
  #### STAI (20 preguntas, ansiedad rasgo)
  # Puntuación:
  #   Hay dos tipos de ítems: “positivos” (presencia de malestar) y “negativos” (ausencia de malestar 
  # o que señalan bienestar). Los ítems negativos tienen puntaje reverso. Para que estén en la “misma 
  # escala de malestar”, en los ítems negativos se invierte el puntaje (PUNTAJE 3 = 0, 2= 1, 1= 2, 0 = 3).
  # Items negativos son los que están en negrita = 1, 6, 7, 10, 13, 16 y 19
  # Puntuación total a utilizar = (positivo directo – negativo invertido) + 20 
  
  
  STAI$pos <- dat_csv$STAI2  + dat_csv$STAI3  + dat_csv$STAI4 + dat_csv$STAI5 + 
    dat_csv$STAI8  + dat_csv$STAI9  + 
    dat_csv$STAI11 + dat_csv$STAI12 + 
    dat_csv$STAI14 + dat_csv$STAI15 + 
    dat_csv$STAI17 + dat_csv$STAI18 + 
    dat_csv$STAI20
  
  STAI$neg <- (3-dat_csv$STAI1)  + (3-dat_csv$STAI6)  + (3-dat_csv$STAI7) + 
    (3-dat_csv$STAI10) + (3-dat_csv$STAI13) + (3-dat_csv$STAI16) + (3-dat_csv$STAI19)
  
  STAI$tot <- STAI$pos - STAI$neg + 20
  
  
  #### SHAI (ansiedad x salud)
  # all groups are scored 0, 1, 2 or 3 depending on the statement selected;
  # if more than one statement is selected, use the highest-scoring statement of those chosen.
  # main section score (questions 1 to 14) =
  # negative consequences score (questions 15 to 18) =
  # total score =
  
  SHAI$pos <- dat_csv$SHAI1  + dat_csv$SHAI2  + dat_csv$SHAI3 + dat_csv$SHAI4 + 
    dat_csv$SHAI5  + dat_csv$SHAI6  + dat_csv$SHAI7 + dat_csv$SHAI8 + 
    dat_csv$SHAI9  + dat_csv$SHAI10  + dat_csv$SHAI11 + dat_csv$SHAI12 + 
    dat_csv$SHAI13  + dat_csv$SHAI14
  
  SHAI$neg <- (3-dat_csv$SHAI5)  + (3-dat_csv$SHAI6)  + (3-dat_csv$SHAI7) + (3-dat_csv$SHAI8) 
  
  SHAI$tot <- SHAI$pos + SHAI$neg 
  
  
  #### STAI
  
  STAIestado$tot <- dat_csv$STAIESTADO1 + dat_csv$STAIESTADO3 + dat_csv$STAIESTADO4 +
    (3-dat_csv$STAIESTADO2)
  

  ## normalizo p1 p2 pT
  p.min       <- c(800000,  150,   0,     0,      0,   0,   5,    0,    0,   0,    0,  0,   0,   0,   0,   0,   0,   0,     0,     0,   10000,  1000000,     0,   0  )
  p.max       <- c(1600000, 195, 100,  100000,   35,  15,  30,   15,   20,   18,  12, 50, 100, 100, 100, 100,  50, 100,    10,    40,  400000,  5000000, 10000,  10  )
  pT          <- c(1260000,  180,  21,  68400,    6,   6,  19,    5,   14,   12,   3, 14,  80,  60,  40,  19,  15,  80,     4,   7.5,  250000,  2200000,  5800,   3.4)
  
  fun_normP <- function(p, pmin, pmax){
    out <- (p - pmin) / (pmax - pmin)
    return(out)
  }
  
  k <- 1
  for( i in seq( which(colnames(dat_csv)=="p1_1"), which(colnames(dat_csv)=="p2_24"), 3) ){
    
    dat_csv[,i]   <- fun_normP(dat_csv[,i], p.min[k], p.max[k])
    dat_csv[,i+1] <- fun_normP(dat_csv[,i+1], p.min[k], p.max[k])
    dat_csv[,i+2] <- fun_normP(dat_csv[,i+2], p.min[k], p.max[k])
    
    k <- k + 1
  }
  
  
  
  
  Npreg <- 24
  d    <- data.frame( subj = rep(NA, Nsuj*Npreg), pregID = rep(NA, Nsuj*Npreg), 
                       p1 = rep(NA, Nsuj*Npreg), pT = rep(NA, Nsuj*Npreg), p2 = rep(NA, Nsuj*Npreg),
                       I = rep(NA, Nsuj*Npreg), ep = rep(NA, Nsuj*Npreg), update = rep(NA, Nsuj*Npreg), 
                       scoreOptimismo = rep(NA, Nsuj*Npreg), 
                       scoreAnsiedad1 = rep(NA, Nsuj*Npreg), 
                       scoreAnsiedad2 = rep(NA, Nsuj*Npreg), 
                       scoreAnsiedad3 = rep(NA, Nsuj*Npreg))
  
  
  
  k <- 1
  for (i in 1:Nsuj){
    
    
    # preg 1
    d$subj[k] <- i
    d$pregID[k]         <- 1
    d$p1[k]             <- dat_csv$p1_1[i]
    d$pT[k]             <- dat_csv$pT_1[i]
    d$p2[k]             <- dat_csv$p2_1[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 2
    d$subj[k] <- i
    d$pregID[k] <- 2
    d$p1[k]     <- dat_csv$p1_2[i]
    d$pT[k]     <- dat_csv$pT_2[i]
    d$p2[k]     <- dat_csv$p2_2[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 3
    d$subj[k] <- i
    d$pregID[k] <- 3
    d$p1[k]     <- dat_csv$p1_3[i]
    d$pT[k]     <- dat_csv$pT_3[i]
    d$p2[k]     <- dat_csv$p2_3[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 4
    d$subj[k] <- i
    d$pregID[k] <- 4
    d$p1[k]     <- dat_csv$p1_4[i]
    d$pT[k]     <- dat_csv$pT_4[i]
    d$p2[k]     <- dat_csv$p2_4[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 5
    d$subj[k] <- i
    d$pregID[k] <- 5
    d$p1[k]     <- dat_csv$p1_5[i]
    d$pT[k]     <- dat_csv$pT_5[i]
    d$p2[k]     <- dat_csv$p2_5[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 6
    d$subj[k] <- i
    d$pregID[k] <- 6
    d$p1[k]     <- dat_csv$p1_6[i]
    d$pT[k]     <- dat_csv$pT_6[i]
    d$p2[k]     <- dat_csv$p2_6[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 7
    d$subj[k] <- i
    d$pregID[k] <- 7
    d$p1[k]     <- dat_csv$p1_7[i]
    d$pT[k]     <- dat_csv$pT_7[i]
    d$p2[k]     <- dat_csv$p2_7[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 8
    d$subj[k] <- i
    d$pregID[k] <- 8
    d$p1[k]     <- dat_csv$p1_8[i]
    d$pT[k]     <- dat_csv$pT_8[i]
    d$p2[k]     <- dat_csv$p2_8[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 9
    d$subj[k] <- i
    d$pregID[k] <- 9
    d$p1[k]     <- dat_csv$p1_9[i]
    d$pT[k]     <- dat_csv$pT_9[i]
    d$p2[k]     <- dat_csv$p2_9[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 10
    d$subj[k] <- i
    d$pregID[k] <- 10
    d$p1[k]     <- dat_csv$p1_10[i]
    d$pT[k]     <- dat_csv$pT_10[i]
    d$p2[k]     <- dat_csv$p2_10[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 11
    d$subj[k] <- i
    d$pregID[k] <- 11
    d$p1[k]     <- dat_csv$p1_11[i]
    d$pT[k]     <- dat_csv$pT_11[i]
    d$p2[k]     <- dat_csv$p2_11[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 12
    d$subj[k] <- i
    d$pregID[k] <- 12
    d$p1[k]     <- dat_csv$p1_12[i]
    d$pT[k]     <- dat_csv$pT_12[i]
    d$p2[k]     <- dat_csv$p2_12[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    # preg 13
    d$subj[k] <- i
    d$pregID[k] <- 13
    d$p1[k]     <- dat_csv$p1_13[i]
    d$pT[k]     <- dat_csv$pT_13[i]
    d$p2[k]     <- dat_csv$p2_13[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 14
    d$subj[k] <- i
    d$pregID[k] <- 14
    d$p1[k]     <- dat_csv$p1_14[i]
    d$pT[k]     <- dat_csv$pT_14[i]
    d$p2[k]     <- dat_csv$p2_14[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 15
    d$subj[k] <- i
    d$pregID[k] <- 15
    d$p1[k]     <- dat_csv$p1_15[i]
    d$pT[k]     <- dat_csv$pT_15[i]
    d$p2[k]     <- dat_csv$p2_15[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 16
    d$subj[k] <- i
    d$pregID[k] <- 16
    d$p1[k]     <- dat_csv$p1_16[i]
    d$pT[k]     <- dat_csv$pT_16[i]
    d$p2[k]     <- dat_csv$p2_16[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 17
    d$subj[k] <- i
    d$pregID[k] <- 17
    d$p1[k]     <- dat_csv$p1_17[i]
    d$pT[k]     <- dat_csv$pT_17[i]
    d$p2[k]     <- dat_csv$p2_17[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 18
    d$subj[k] <- i
    d$pregID[k] <- 18
    d$p1[k]     <- dat_csv$p1_18[i]
    d$pT[k]     <- dat_csv$pT_18[i]
    d$p2[k]     <- dat_csv$p2_18[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 19
    d$subj[k] <- i
    d$pregID[k] <- 19
    d$p1[k]     <- dat_csv$p1_19[i]
    d$pT[k]     <- dat_csv$pT_19[i]
    d$p2[k]     <- dat_csv$p2_19[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 20
    d$subj[k] <- i
    d$pregID[k] <- 20
    d$p1[k]     <- dat_csv$p1_20[i]
    d$pT[k]     <- dat_csv$pT_20[i]
    d$p2[k]     <- dat_csv$p2_20[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 21
    d$subj[k] <- i
    d$pregID[k] <- 21
    d$p1[k]     <- dat_csv$p1_21[i]
    d$pT[k]     <- dat_csv$pT_21[i]
    d$p2[k]     <- dat_csv$p2_21[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 22
    d$subj[k] <- i
    d$pregID[k] <- 22
    d$p1[k]     <- dat_csv$p1_22[i]
    d$pT[k]     <- dat_csv$pT_22[i]
    d$p2[k]     <- dat_csv$p2_22[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 23
    d$subj[k] <- i
    d$pregID[k] <- 23
    d$p1[k]     <- dat_csv$p1_23[i]
    d$pT[k]     <- dat_csv$pT_23[i]
    d$p2[k]     <- dat_csv$p2_23[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    # preg 24
    d$subj[k] <- i
    d$pregID[k] <- 24
    d$p1[k]     <- dat_csv$p1_24[i]
    d$pT[k]     <- dat_csv$pT_24[i]
    d$p2[k]     <- dat_csv$p2_24[i]
    d$I[k]              <- ifelse( d$p1[k] > d$pT[k], 1, 0 )
    d$ep[k]             <- abs( d$p1[k] - d$pT[k] )
    d$update[k]         <- ifelse(d$p1[k] > d$pT[k], d$p1[k]-d$p2[k],  d$p2[k]-d$p1[k])
    
    d$scoreOptimismo[k] <- LOTR$tot[i]
    d$scoreAnsiedad1[k] <- STAI$tot[i]
    d$scoreAnsiedad2[k] <- SHAI$tot[i]
    d$scoreAnsiedad3[k] <- STAIestado$tot[i]
    
    k <- k + 1
    
    
    
  }
  
  
  d$scoreOptimismo_std      <- ( d$scoreOptimismo      - mean(d$scoreOptimismo) )      / sd(d$scoreOptimismo)
  d$scoreAnsiedad1_std      <- ( d$scoreAnsiedad1      - mean(d$scoreAnsiedad1) )      / sd(d$scoreAnsiedad1)
  d$scoreAnsiedad2_std      <- ( d$scoreAnsiedad2      - mean(d$scoreAnsiedad2) )      / sd(d$scoreAnsiedad2)
  d$scoreAnsiedad3_std      <- ( d$scoreAnsiedad3      - mean(d$scoreAnsiedad3) )      / sd(d$scoreAnsiedad3)
  
  d$ep_std <- (d$ep - mean(d$ep)) / sd(d$ep)
  d$pT_std <- (d$pT - mean(d$pT)) / sd(d$pT)
  
  
  d$subj    <- factor(d$subj)
  d$pregID  <- factor(d$pregID)
  
  return(d)
}
