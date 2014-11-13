## File cachematrix.R
## Functions
## 1. makeCacheMatrix
## 2. cacheSolve
## The above functions compute and cache the inverse of a matrix.
## Repeated calls return the cached results without recomputing.


## Function makeCacheMatrix build and returns a list with functions
## 1. setMatrix - set the value of the matrix
## 2. getMatrix - get the value of the matrix
## 3. setInverseMatrix - set the inverse of the matrix in parent(global) environment
## 4. getInverseMatrix - get the inverse of matrix if availble in the parent 
##                       environment else return NULL.      

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
            
      ## set matrix
      setMatrix <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## return matrix 
      getMatrix <- function() x
      
      ## set cache inverse
      setInverseMatrix <- function(inv) {
            inverse <<- inv
      }
      
      ## return cache inverse
      getInverseMatrix <- function() inverse
      
      list(setMatrix = setMatrix, 
           getMatrix = getMatrix,
           setInverseMatrix = setInverseMatrix, 
           getInverseMatrix = getInverseMatrix)
}


## Function cacheSolve solves the matrix in makeCacheMatrix and returns its inverse
## It checks if the inverse is in cache. Else it computes and stores in cache
## Subsequent calls return the inverse from the cache

cacheSolve <- function(x, ...) {
      
      ## Retrieve a matrix that is the inverse of 'x' from cache
      inverse <- x$getInverseMatrix()
      
      ## if value in cache is null compute inverse and store in cache
      if(is.null(inverse)) {
            
            message("adding new matrix to cache")
            matrix <- x$getMatrix()
            inverse <- solve(matrix, ...)
            x$setInverseMatrix(inverse)
      }
      else {
            message("getting inverse from cache")
      }
      
      ## inverse returned from cache or added to cache and returned
      inverse
}
