add2 <- function(x,y) {
      x+y
}

above10 <- funciton(x) {
      use <- x > 10
      x[use]
}

above <- function(x, n) {
      use <- x > n
      x[use]
}

columnmean <- function(y, removeNA) {
      nc <- ncol(y)
      means <- numeric(nc)
      
      for(i in 1:nc) {
            means[i] <- mean(y[,i], na.rm = removeNA)
      }
      means
}

cube <- function(x, n) {
      x^3
}
test <- function() {

      x <- 1:10
      if(x > 5) {
            x <- 0
      }
}

test2 <- function(x) {
      g <- function(y) {
            y + z
      }
      z <- 4
      x + g(x)
}