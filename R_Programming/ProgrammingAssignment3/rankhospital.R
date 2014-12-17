source("best.R")
rankhospital <- function(state, outcome, num) {
      
      outcomes <- readvaliddata(state, outcome)
      needed.outcomes <- collectneededoutcomes(outcomes, state, outcome)
      
      sorted.outcomes <- needed.outcomes[order(needed.outcomes[, outcome], needed.outcomes[, "Hospital Name"]), ]
      
      rank <- seq(nrow(sorted.outcomes))
      sorted.outcomes <- cbind(sorted.outcomes, rank)
      if(num == "worst") num <- max(rank)
      if(num == "best") num <- 1
      result <- sorted.outcomes[sorted.outcomes[, "rank"] == num, ]
      
      if(nrow(result) == 0 ) return (NA)
      return (as.character(result[1]))
}