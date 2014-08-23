## School: Coursera / John Hopkins
## Course: R Programming (rprog-006)
## Assignment: Programming Assignment 2
## Instructor: Roger D. Peng
## Student: Joshua Poirier
## Date: August 24, 2014

## makeCacheMatrix creates a special "matrix" object capable of caching its
## inverse matrix (via the cacheSolve function)

## makeCacheMatrix creates a special "matrix", actually a list containing a 
## function which:
##      1 - sets the value of the matrix
##      2 - gets the value of the matrix
##      3 - sets the value of the inverted matrix
##      4 - gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    ## reset cached inverse matrix
    
    ## cache matrix set function
    set <- function(y) {
        x <<- y
        m <<- NULL    ## reset cached inverse matrix
    }
    
    ## retrieve data matrix
    get <- function() x
    
    ## set the cached inverse matrix
    setInverse <- function(solve) m <<- solve
    
    ## retrieve cached inverse matrix
    getInverse <- function() m
    
    ## return cache matrix object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inversion of the special "matrix" created with the 
## makeCacheMatrix function.  It first checks to see if the inversion has already
## been computed - if it has been computed, it gets the inverted matrix from the
## cache and skips the computation.  If it has not been computed, it computes the 
## inverse matrix via the "solve" function.
cacheSolve <- function(x, ...) {
        ## retrieve inverse matrix from memory
        m <- x$getInverse()
        
        ## check if inverse matrix has been set, return if so
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        ## compute, set, and return inverse matrix
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
