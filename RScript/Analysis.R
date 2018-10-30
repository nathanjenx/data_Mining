library(e1071)
library(foreach)

plotTrueMatrices <- function(inst, instrument){
  drops <- c("mix1_instrument", "mix2_instrument")
  quartz(title= "Pairs Plots", width=28, height=20)
  View(inst[[instrument]]$t)
  #plot(inst[[instrument]]$t[, !(names(inst[[instrument]]$t) %in% drops)])
}

nbmTrainV1 <- function(df){
  model = naiveBayes(Instrument ~., data=df)
  return(model)
}

nbmPredictV1 <- function(df, model){
  predictions=predict(model,df)
  return(predictions)
}

nbmTblV1 <- function(df, predictions){
  tbl <- table(predictions, real = df$instrument)
  
  tN <- as.numeric(tbl[1])
  fN <- as.numeric(tbl[3])
  
  tP <- as.numeric(tbl[4])
  fP <- as.numeric(tbl[2])
  
  tNAccuracy <- (tN/(tN + fN)) * 100
  tPAccuracy <- (tP/(tP + fP)) * 100
  accuracy <- ((tP + tN)/(tP + tN + fP + fN)) * 100
  list <- list(tNAccuracy = tNAccuracy, tPAccuracy = tPAccuracy, accuracy = accuracy)
  return(list)
}

nbmV1 <- function(df) {
  model <- nbmTrainV1(df)
  predictions <- nbmPredictV1(df, model)
  accuracy <- nbmTblV1(df, predictions)
  return(accuracy)
}

nbmModel <-function(trainDf, testDf) {
  model <- naiveBayes(Instrument ~., data=trainDf)
  predictions <- predict(model,testDf)
  return(predictions)
}


geNBMModels <- function (trainDf, testDf, instruments){
  trainDf <- cleanData(trainDf)
  testDf <- cleanData(testDf)
  predsRows <- c("inst", "predictions")
  preds <- list()#data.frame(row.names = predsRows)
  for(i in 1:nrow(instruments)){
    inst <- instruments[i, 1]
    trainDfBool <- createBooleanInstrument(trainDf, inst)
   # testDf <- createBooleanInstrument(testDf, inst)
    predsByInst <- nbmModel(trainDfBool,testDf)
    #predsDf <- data.frame(inst, predsByInst, row.names = predsRows)
    preds[[i]] <- data.frame(predsByInst)
  }
  return(preds)
}

getNBMByclass <- function (trainDf, testDf, map){
  testDf <- cleanData(testDf)
  trainDf <- cleanData1(trainDf)
  predsRows <- c("inst", "predictions")
  preds <- list()
  i <- 1
  for(class1 in seq_along(map)){
    for(class2 in seq_along(map[[class1]])){
      for(inst in seq_along(map[[class1]][[class2]])){
        instrument <- map[[class1]][[class2]][[inst]]
        class2 <- names(map[[class1]][class2])
        trainDfBool <- createBooleanInstrumentByClass2(trainDf, instrument, class2)
        trainDfBool <- cleanData2(trainDfBool)
        predsByInst <- nbmModel(trainDfBool,testDf)
        preds[[i]] <- data.frame(predsByInst)
        i <- i + 1
      }
    }
  }
  return(preds)
}

getAccuracyForAllnbmV1 <- function(df, mixes){
  accuracysRows <- c("Instrument", "Positive", "Negative", "Overall")
  accuracys <- data.frame(row.names = accuracysRows)
  for(mix in mixes){
    dfBool <- createBooleanInstrument(df, mix)
    accuracy <- nbmV1(dfBool)
    accuracy <- data.frame(mix, accuracy$tPAccuracy, accuracy$tNAccuracy, accuracy$accuracy)
    names(accuracy) <- accuracysRows
    accuracys <- rbind(accuracys, accuracy)
  }
  return(accuracys)
}

analyseByClass <- function(df){
  
 # colnames(byClass) = c("Id", "Instrument")
  
  #by(df, 1:nrow(df), function(row){
  #  split <- c(rowNo, "?")
  #  if(df$class1 == "chordophone"){
  #    split <- splitByChordophone(row, rowNo)
  #  }else if(df$class1 == "aerophone"){
  #    split <- splitByAerophone(row, rowNo)
  #  }
  #  rowNo <- rowNo + 1
  #  rbind(byClass, split)
  #})
  
  names <- c("Id", "Instrument")
  byClass <- data.frame()

  for(i in 1:nrow(df)){
    row <- df[i,]
    if (row$class1 == "chordophone") {
      split <- splitByChordophone(row, i)
    } else if(row$class1 == "aerophone"){
      split <- splitByAerophone(row, i)
    }
    split <- data.frame(Id = split[1], Instrument = split[2])
    byClass <- rbind(byClass, split)
    #byClass[i, Instrument] = split[2]
  }
  return(byClass)
}

splitByChordophone <- function(row, rowNo){
  #More nesting??? e.g go as far as class2??
  inst <- "??"
  if(row$playmethod == "string"){
    inst <- "?"
  } else if(row$playmethod == "struck_Hrm"){
    inst <- "Piano"
  }
  splitVec <- c(rowNo, inst)
  return(splitVec)
}

splitByAerophone <- function(row, rowNo){
  #More nesting??? e.g go as far as class2??
  if(row$class2 == "aero_single-reed"){
    inst <- "?"
  } else if(row$class2 == "aero_side"){
    inst <- "?"
  } else if(row$class2 == "aero_lip-vibrated"){
    inst <- "?"
  } else if(row$class2 == "aero_free-reed"){
    inst <- "Accordian"
  } else if(row$class2 == "aero_double-reed"){
    inst <- "?"
  }
  splitVec <- c(rowNo, inst)
  return(splitVec)
}


generatePredictions <- function(df){
  rows <- c("Id", "Instrument")
  p <- data.frame(row.names = rows)

  for(i in 1:nrow(df)){
    row <- df[i,]
    if(row$class1 == "chordophone"){
      inst <- predictChordophone(row)
    } else if(row$class1 == "aerophone"){
      inst <- predictAerophone(row)
    }
    n <- data.frame(row$Id, inst)
    names(n) <- rows
    p <- rbind(p, n)
  }
  return(p)
}

predictAerophone <- function(row){
  inst <- "a"
  if(row$class2 == "aero_free-reed"){
    inst <- "Accordian"
  } else if(row$class2 == "aero_side"){
    inst <- predictAeroSide(row)
  } else if(row$class2 == "aero_lip-vibrated"){
    inst <- predictAeroLip(row)
  } else if(row$class2 == "aero_single-reed"){
    inst <- predictAeroSingle(row)
  } else if(row$class2 == "aero_double-reed"){
    inst <- predictAeroDouble(row)
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

predictAeroSide <- function(row){
  inst <- "b"
  #if(multiCheck(c(row$Flute,row$Piccolo)) == TRUE){
    inst <- "Multiple: Flute/Piccolo"
  #}else if(row$Piccolo == TRUE){
  #  inst <- "Piccolo"
  #} else if(row$Flute == TRUE){
  #  inst <- "Flute"
  #} else {
    inst <- paste0("No match found: ",inst)
#  }
  return(inst)
}

predictAeroLip <- function(row){
  inst <- "c"
  if(multiCheck(c(row$Tuba, row$Trumpet, row$Trombone)) == TRUE){
    inst <- "Multiple: Tuba/Frenchhorn/Trumpet/Trombone"
  }else if(row$Tuba == TRUE){
    inst <- "Tuba"
  #} else if(row$Frenchhorn == TRUE){
   # inst <- "Frenchhorn"
  } else if(row$Trumpet == TRUE){
    inst <- "Trumpet"
  } else if(row$Trombone == TRUE){
    inst <- "Trombone"
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

predictAeroSingle <- function(row){
  inst <- "d"
  if(multiCheck(c(row$Clarinet, row$Saxophone)) == TRUE){
    inst <- "Multiple: Clarinet/Saxophone"
  }else if(row$Clarinet == TRUE){
    inst <- "Clarinet"
  } else if(row$Saxophone == TRUE){
    inst <- "Saxophone"
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

predictAeroDouble <- function(row){
  inst <- "e"
 # if(multiCheck(c(row$Oboe)) == TRUE){
    inst <- "Multiple: Bassoon/Oboe/EnglishHorn"
  #}else if(row$Bassoon == TRUE){
  #  inst <- "Bassoon"
   if(row$Oboe == TRUE){
    inst <- "Oboe"
  #} else if(row$EnglishHorn == TRUE){
    #inst <- "EnglishHorn"
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

predictChordophone <- function(row){
  #More nesting??? e.g go as far as class2??
  inst <- "f"
  if(row$class2 == "chrd_simple"){
    inst <- "Piano"
  } else if(row$class2 == "chrd_composite"){
    inst <- predictChordComposite(row)
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

predictChordComposite <- function(row){
  inst <- "g"
  if(multiCheck(c(row$Guitar, row$Cello, row$DoubleBass, row$Violin, row$Viola)) == TRUE){
    inst <- "Multiple: Guitar/Cello/DoubleBass/Violin/Viola/SynthBass"
  }else if(row$Guitar == TRUE){
    inst <- "Guitar"
  } else if(row$Cello == TRUE){
    inst <- "Cello"
  } else if(row$DoubleBass == TRUE){
    inst <- "DoubleBass"
  } else if(row$Violin == TRUE){
    inst <- "Violin"
  } else if(row$Viola == TRUE){
    inst <- "Viola"
  #} else if(row$SynthBass == TRUE){
    #inst <- "SynthBass"
  } else {
    inst <- paste0("No match found: ",inst)
  }
  return(inst)
}

multiCheck <- function(items){
  i <- 0
  ret <- FALSE
  for(item in items){
    if (item == TRUE){
      i <- i + 1
    }
    if(i > 1){
      ret <- TRUE
      break
    }
  }
  
  return(ret)
}
