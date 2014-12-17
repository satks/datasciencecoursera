library(data.table)
rankall <- function(outcome, num = "best") {
      
      outcomes <- fread("outcome-of-care-measures.csv", select = c(2, 7, 11, 17, 23), colClasses = "character")
      
      names(outcomes) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
      
      if(!(tolower(outcome) %in% colnames(outcomes[3:5]))) stop ("invalid outcome")
      
      needed.outcomes[, tolower(outcome)] <- as.numeric(needed.outcomes[, tolower(outcome)])
      needed.outcomes <- needed.outcomes[complete.cases(needed.outcomes), ]
      print(dim(needed.outcomes))
      
      sorted.outcomes <- needed.outcomes[order(needed.outcomes[, outcome], needed.outcomes[, "hospital"]), ]
      rank <- seq(nrow(sorted.outcomes))
      sorted.outcomes <- cbind(sorted.outcomes, rank)
      
      if(num == "worst") num <- max(rank)
      if(num == "best") num <- 1
            
      result <- sorted.outcomes[sorted.outcomes[, "rank"] == num, ]
      
      return (result)
}