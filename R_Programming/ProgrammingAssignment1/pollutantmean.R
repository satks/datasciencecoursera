pollutantmean <- function(directory, pollutant, id = 1:332) {
      
      allfiles <- dir(directory, pattern = '\\.csv', full.names = TRUE)
      
      allpollutantdata <- lapply(allfiles, read.csv)
      alldataastable <- do.call(rbind, allpollutantdata)

      requiredpollutant <- subset(alldataastable, ID >= min(id) & ID <= max(id), select = as.character(pollutant))
      
      pollutantmean <- mean(requiredpollutant[,as.character(pollutant)], na.rm = TRUE)
      
      pollutantmean
}