library(readr)
library(sqldf)
library(foreach)

getMixes <- function(data){
  unq <- unique(c(data$mix1_instrument, data$mix2_instrument))
  ord <- unq[order(unq)]
  return(ord)
}

selectInstrument <- function(instrument, equal = TRUE, data){
  mix1 <- 'SELECT * FROM data where mix1_instrument '
  mix2 <- " mix2_instrument "
  instrument <- paste0(' "', instrument, '" ')
  if(equal){
    equal <- "="
    mix1 <- paste0(mix1, equal)
    mix2 <- paste0("OR", mix2, equal)
  } else {
    notEqual <- "!="
    mix1 <- paste0(mix1, notEqual)
    mix2 <- paste0("AND", mix2, notEqual)
  }
  sql <- paste0(mix1, instrument, mix2, instrument)
  #View(sql)
  selection <- sqldf(sql)
  return(selection)
}

getInstruments <- function(data, mixes, inst = FALSE){
 instruments <- list()
 if(inst != FALSE){
   mixes <- c(inst)
 }
 for(mix in mixes){
   instruments[[mix]] <- list(
     t = selectInstrument(mix, TRUE, data),
     f = selectInstrument(mix, FALSE, data)
     )
 }
 return(instruments)
}

importData <- function(path){
  df <- data.frame(read_csv(path))
  cols <- c("HamoPk1", "HamoPk2", "HamoPk3", "HamoPk4", "HamoPk5", "HamoPk6", 
            "HamoPk7", "HamoPk8", "HamoPk9", "HamoPk10", "HamoPk11", "HamoPk12", 
            "HamoPk13", "HamoPk14", "HamoPk15", "HamoPk16", "HamoPk17", "HamoPk18", 
            "HamoPk19", "HamoPk20", "HamoPk21", "HamoPk22", "HamoPk23", "HamoPk24", 
            "HamoPk25", "HamoPk26", "HamoPk27", "HamoPk28")
  
  for(col in cols){
    df[, col] <- as.numeric(df[,col])
  }
  
  print("Data Loaded!")
  
  return(df)
}

getIndividualTracks <- function(inst){
  mix1Df <- selectDataWithMix(inst, 1)
  mix2Df <- selectDataWithMix(inst, 2)
  combined <- rbind(mix1Df, mix2Df)
  return(combined)
}

selectDataWithMix <- function(inst, mixNo){
  mix <- paste0('mix', mixNo, '_instrument')
  
  if(mixNo %% 2 == 0){
    mix2Remove <- 'mix1_instrument'
  } else {
    mix2Remove <- 'mix2_instrument'
  }
  
  sqlPt1 <- 'SELECT * FROM inst WHERE ' 
  sqlPt2 <- ' != "?"'
  
  sql <- paste0(sqlPt1, mix, sqlPt2)

  df <- sqldf(sql)
  df <- removeCols(df, c(mix2Remove))
  
  names(df)[names(df) == mix] <- "instrument"
  
  return(df)
}

removeCols <- function(df, cols){
 return(df[, !(names(df) %in% cols)]) 
}

