source("~/Documents/Uni/Data Mining/Project/RScript/Data.R")
source("~/Documents/Uni/Data Mining/Project/RScript/Analysis.R")

trainingData <- importTrainingData("~/Documents/Uni/Data Mining/Project/Original Data Download/all/train.csv")
mixes <- getMixes(trainingData)
#testData <- importTestData("~/Documents/Uni/Data Mining/Project/Original Data Download/all/test.csv")
testData <- importTrainingData("~/Documents/Uni/Data Mining/Project/Extra Data/testInstrumentCol.csv")

extraTrainingData <- importTrainingData("~/Documents/Uni/Data Mining/Project/Extra Data/singleInstrumentsTrain.csv")
instruments <- getDistinctInstruments(extraTrainingData)



getByInstrument <- function(){
inst <- getInstruments(trainingData, mixes, "Accordian")
}

# plotTrueMatrices(inst, "Accordian") ----- Too many data points to run pairs on
getNbmAccuracy <- function(){
  singleTrack <- getIndividualTracks(trainingData)
  cleanedMixes <- mixes[!mixes %in% c("?")]
  accuracys <- getAccuracyForAllnbmV1(singleTrack, cleanedMixes)
  return(accuracys)
}

mixes2 <- function(){
  rows <- c("Instrument", "Class1")
  mixes <- data.frame(row.names = rows)
  df <- data.frame(row.names = rows)
  
  df <- data.frame(Instrument = "Accordian", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "AcousticBass", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "AltoSaxophone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "B-flatclarinet", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "B-FlatTrumpet", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "BaritoneSaxophone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "BassSaxophone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Cello", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "CTrumpet", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "DoubleBass", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "DTrumpet", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "ElectricGuitar", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Marimba", Class1 = "precussion")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Oboe", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Piano", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "SopranoSaxophone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "TenorSaxophone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "TenorTrombone", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Tuba", Class1 = "aerophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Vibraphone", Class1 = "idiophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Viola", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
  
  df <- data.frame(Instrument = "Violin", Class1 = "chordophone")
  mixes <- rbind(mixes, df)
}

mapClasses <- function(){
  rows <- c("Class1", "Class2")
  classes <- data.frame(row.names = rows)
  
  df <- data.frame(Class1 = "chordophone", Class2 = "chrd_simple")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "chordophone", Class2 = "chrd_composite")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "aerophone", Class2 = "aero_lip-vibrated")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "aerophone", Class2 = "aero_side")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "aerophone", Class2 = "aero_single-reed")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "aerophone", Class2 = "aero_double-reed")
  classes <- rbind(classes, df)
  
  df <- data.frame(Class1 = "aerophone", Class2 = "aero_free-reed")
  classes <- rbind(classes, df)
}

mapInst2Class <- function(){
  names <- c("Instrument", "Class2")
  inst2Class <- data.frame(row.names = names)
  inst2Class <- map2Df(names, "Accordian", "aero_free-reed", inst2Class)
  inst2Class <- map2Df(names, "AcousticBass", "chrd_composite", inst2Class)
  inst2Class <- map2Df(names, "AltoSaxophone", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "B-flatclarinet", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "B-FlatTrumpet", "aero_lip-vibrated", inst2Class)
  inst2Class <- map2Df(names, "BaritoneSaxophone", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "BassSaxophone", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "Cello", "chrd_composite", inst2Class)
  inst2Class <- map2Df(names, "CTrumpet", "aero_lip-vibrated", inst2Class)
  inst2Class <- map2Df(names, "DoubleBass", "chrd_composite", inst2Class)
  inst2Class <- map2Df(names, "DTrumpet", "aero_lip-vibrated", inst2Class)
  inst2Class <- map2Df(names, "ElectricGuitar", "chrd_composite", inst2Class)
  #inst2Class <- map2Df(names, "Marimba", "precussion", inst2Class)
  inst2Class <- map2Df(names, "Oboe", "aero_double-reed", inst2Class)
  inst2Class <- map2Df(names, "Piano", "chrd_simple", inst2Class)
  inst2Class <- map2Df(names, "SopranoSaxophone", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "TenorSaxophone", "aero_single-reed", inst2Class)
  inst2Class <- map2Df(names, "TenorTrombone", "aero_side", inst2Class)
  inst2Class <- map2Df(names, "Tuba", "aero_lip-vibrated", inst2Class)
  #inst2Class <- map2Df(names, "Vibraphone", "idiophone", inst2Class)
  inst2Class <- map2Df(names, "Viola", "chrd_composite", inst2Class)
  inst2Class <- map2Df(names, "Violin", "chrd_composite", inst2Class)
      
  return(inst2Class)
}

map2Df <- function (names, d1, d2, outDf){
  df <- data.frame(d1, d2)
  names(df) <- names
  return(rbind(outDf, df))
}

generatePredictionsFromNbmModel <- function(){
  predictionsFull <- geNBMModels(extraTrainingData, testData, instruments)
  pClean <- predicitionsCleansed(testData, predictionsFull)
  prediction <- generatePredictions(pClean)
  write_csv(prediction, "~/Desktop/rPredictAll2.csv")
}