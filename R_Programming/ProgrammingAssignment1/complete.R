complete <- function(directory, id = 1:332) {
      calculatecompletecases(readcleandata(directory, id), id)
}

readcleandata <- function(directory, id = 1:332) {
      ids <- formatC(id, width = 3, format = "d", flag = "0")
      filelist <- paste(directory, "/", ids, ".csv", sep = "")
      allpollutantdata <- lapply(filelist, read.csv)
      
      dataastable <- rbindlist(allpollutantdata)
      dataastable <- dataastable[complete.cases(dataastable), ]
}

calculatecompletecases <- function(dataastable, id) {
      completeitems <- aggregate(dataastable$ID, list(dataastable$ID), length)
      names(completeitems) <- c("id", "nobs")
      completeitems <- completeitems[match(id, completeitems$id), ]
      completeitems
}