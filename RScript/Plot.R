plotTrueMatrices <- function(inst, instrument){
  drops <- c("mix1_instrument", "mix2_instrument")
  quartz(title= "Pairs Plots", width=28, height=20)
  View(inst[[instrument]]$t)
  #plot(inst[[instrument]]$t[, !(names(inst[[instrument]]$t) %in% drops)])
}