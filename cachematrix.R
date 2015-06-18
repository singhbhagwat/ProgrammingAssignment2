## *Important assumption : Considering assignment instructions only invertible matrices should be 
##                         used to test the result as no exception handling is done for 
##                         non invertible matrices

## Function makeCacheMatrix creates a special Matrix object that can cache its Inverse, 
## allows to set Matrix, get currently set Matrix, set Inverse and get current Matrix's Inverse.

## Functions cacheSolve returns Inverse of the matrix contained in special Matrix Object passed 
## to it.

## makeCacheMaytix creates a special Matrix object that can cache its Inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y = matrix()) { 
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    
    setInverse <- function(Inverted) inverse <<- Inverted
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## Special matrix object created using makeCacheMatrix to store last seen matrix.
previousSpecialMatrix <- makeCacheMatrix()


## cacheSolve returns Inverse of the matrix contained in special Matrix Object passed 
## to it. It first checks if current matrix is identical to last seen matrix, if it is 
## it retrievs the Inverse Matrix from the cache, else it solves for Inverse and return that.
cacheSolve <- function(X) {
    
    data <- X$get()
    prevData <- previousSpecialMatrix$get()
    
    
    if(identical(prevData, data)) {
        message("getting cached data")
        
        i <- X$getInverse()
        prevInverse <- previousSpecialMatrix$getInverse()
        
        if(identical(prevInverse, i)) return(i)
        else {
            X$setInverse(prevInverse)
            return(prevInverse)
        }
    }
    
    i <- solve(data)
    X$setInverse(i)
    
    previousSpecialMatrix <<- X
    
    i
}
