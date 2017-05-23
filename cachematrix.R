## This file contains the two functions 'makeCacheMatrix' and 'cacheSolve'


## makeCacheMatrix
#  This function creates a 'special matrix object' that can cache the 
# inverseof a matrix. The special object can be accessed with the 
# functions get(), set(), getInv() and setInv(). These functions allow 
# the setting and getting of the data and inverse of the matrix,
# respectively.
makeCacheMatrix <- function(x = matrix()) {
          # set inverse to null, untill defined
          inv <- NULL
          
          # set matrix data
          set <- function(y) {
                    x <<- y
                    inv <<- NULL
          }
          
          # function to return matrix
          get <- function() x
          
          # function to set inverse
          setInv <- function(inverse) inv <<- inverse
          
          # function to get inverse
          getInv <- function() inv
          
          list(set = set, get = get,
               setInv = setInv,
               getInv = getInv)
}


## cacheSolve
# This function calculates the inverse of the matrix object that is
# created with the function 'makeCacheMatrix'. If the inverse has 
# already been cached, then that cached data is returned. Otherwise,
# the inverse is calculated and the new inverse is stored in the
# 'special matrix object' using setInv().
cacheSolve <- function(x, ...) {
          
          # get inverse of matrix
          inv <- x$getInv()
          
          # if inverse already calculated, get cached data and
          # return inverse. Rest of function not executed.
          if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
          }
          
          # If getInv() returns NULL, inverse not yet calculated
          
          data <- x$get()               # get matrix
          inv <- solve(data, ...)       # calculate inverse
          x$setInv(inv)                 # set inverse in special object
          inv                           # return inverse of matrix
}





