corr <- function(directory, threshold = 0) {
      
      id <- 1:332
      getcleandata <- readcleandata(directory)
      completedlist <- calculatecompletecases(getcleandata, id)
      completedlist <- completedlist[complete.cases(completedlist), ]
      listabovethreshold <- completedlist[completedlist$nobs >= threshold, ]$id
      
      corvar <- sapply(listabovethreshold, correlator, 
             data = getcleandata)
      corvar
}

correlator <- function(id, data) {
      plotdata <- data[data$ID == id, ]
      corvar <- cor(plotdata$sulfate, plotdata$nitrate)
      corvar
}