best <- function(state, outcome) {
      
      outcomes <- readvaliddata(state, outcome)
               
      needed.outcomes <- collectneededoutcomes(outcomes, state, outcome)
            
      lowest <- min(needed.outcomes[, outcome])
      
      result <- needed.outcomes[needed.outcomes[,outcome] == lowest, ]
      
      if(nrow(result) == 1) return (as.character(result[1]))
      else {
            sorted <- order(result[, "Hospital Name"])
            return (result[sorted,][1,1])
      }
      
}

readvaliddata <- function(state = "ALL", outcome) {
      outcomes <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character", 
                           na.strings = c("Not Availble"))[c(2, 7, 11, 17, 23)]
      names(outcomes) <- c("Hospital Name", "state", "heart attack", "heart failure", "pneumonia")
      if(!(toupper(state) %in% outcomes$state)) stop ("invalid state")
      if(!(tolower(outcome) %in% colnames(outcomes[3:5]))) stop ("invalid outcome")
      
      return (outcomes)
}

collectneededoutcomes <- function(outcomes, state, outcome) {
      
      needed.outcomes <- outcomes[outcomes$state == toupper(state), ]
      
      needed.outcomes[, tolower(outcome)] <- as.numeric(needed.outcomes[, tolower(outcome)])
      needed.outcomes <- needed.outcomes[complete.cases(needed.outcomes), ]
}