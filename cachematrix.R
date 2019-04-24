## Caching the inverse of a matrix

## The function "makeCacheMatrix" creates a special "matrix" object that can cache
## its inverse. It takes as input a matrix, and
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse 
## 4. gets the value of the inverse 

## <<- operator can be used to assign a value to an object in an environment that
## is different from the current environment 

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL                   
        setmatrix <- function(y){
                x <<- y
                invmatrix <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) invmatrix <<- solve
        getinverse <- function() invmatrix
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinvere = getinverse)
}


## The function "cacheSolve" takes the output of the previous "matrix" created
## by "makeCacheMatrix." It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value in the
## cache via the "setinverse" function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)){
                message("getting cached data")
                return(invmatrix)
        }
        matrixdata <- x$getmatrix()
        invmatrix <- solve(matrixdata,...)
        x$setinverse(invmatrix)
        invmatrix
}
