library(readr)
library(sqldf)
library(foreach)

getMixes <- function(data){
  unq <- unique(c(data$mix1_instrument, data$mix2_instrument))
  ord <- unq[order(unq)]
  return(ord)
}
getDistinctInstruments <- function(df){
  return(sqldf("SELECT DISTINCT Instrument FROM df ORDER BY Instrument"))
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

importTrainingData <- function(path){
  df <- importTestData(path)
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

cleanData <- function(df){
  cols <- c("HamoPk1", "HamoPk2", "HamoPk3", "HamoPk4", "HamoPk5", "HamoPk6", 
    "HamoPk7", "HamoPk8", "HamoPk9", "HamoPk10", "HamoPk11", "HamoPk12", 
    "HamoPk13", "HamoPk14", "HamoPk15", "HamoPk16", "HamoPk17", "HamoPk18", 
    "HamoPk19", "HamoPk20", "HamoPk21", "HamoPk22", "HamoPk23", "HamoPk24", 
    "HamoPk25", "HamoPk26", "HamoPk27", "HamoPk28")
  
  for(col in cols){
    df[, col] <- as.numeric(df[,col])
  }
  
  cols2remove <- c("note", "playmethod",	"class1",	"class2")
  df <- removeCols(df, cols2remove)
  
  return(df)
}

importTestData <- function(path){
  df <- data.frame(read_csv(path))
  print("Data Read from CSV")
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
  
  names(df)[names(df) == mix] <- "Instrument"
  
  return(df)
}

createBooleanInstrument <- function(df, inst){
  trueDf <- selectForInstrument(df, inst, TRUE)
  falseDf <- selectForInstrument(df, inst, FALSE)
  comb <- rbind(trueDf, falseDf)
  return(comb)
}

selectForInstrument <- function (data, inst, equal = TRUE){
  equality <- "="
  if(equal == FALSE){
    equality = "!="
  }
  instSql <- paste0('"', inst, '"')
  sql <- paste("SELECT * FROM data WHERE Instrument", equality, instSql, sep = " ")
  print(sql)
  df <- sqldf(sql)
  df <- booleanCol(df, equal)
  return(df)
}

booleanCol <- function(df, val){
  df$Instrument <- val
  return(df)
}

removeCols <- function(df, cols){
 return(df[, !(names(df) %in% cols)]) 
}

predicitionsCleansed <- function(testData, predictions){
  rows <- c("Id", "playmethod", "class1", "class2", instruments[[1, 1]], instruments[[2, 1]],
            instruments[[3, 1]], instruments[[4, 1]], instruments[[5, 1]], instruments[[6, 1]],
            instruments[[7, 1]], instruments[[8, 1]], instruments[[9, 1]], instruments[[10, 1]],
            instruments[[11, 1]], instruments[[12, 1]], instruments[[13, 1]], instruments[[14, 1]],
            instruments[[15, 1]], instruments[[16, 1]], instruments[[17, 1]], instruments[[18, 1]],
            instruments[[19, 1]])
  p <- data.frame(row.names = rows)
  for(i in 1:nrow(testData)){
    o <- testData[i,]
    n <- predictions
    temp <- data.frame(i, o$playmethod, o$class1, o$class2,
                       n[[1]][i,], n[[2]][i,], n[[3]][i,], 
                       n[[4]][i,], n[[5]][i,], n[[6]][i,], 
                       n[[7]][i,], n[[8]][i,], n[[9]][i,], 
                       n[[10]][i,], n[[11]][i,], n[[12]][i,], 
                       n[[13]][i,], n[[14]][i,], n[[15]][i,], 
                       n[[16]][i,], n[[17]][i,], n[[18]][i,], 
                       n[[19]][i,])
    names(temp) <- rows
    p <- rbind(p, temp)
  }
  return(p)
}
