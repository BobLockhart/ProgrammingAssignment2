## R Programming Assignment 2 contains two functions:  makeCacheMatrix & cacheSolve 
## makeCacheMatrix:  takes in a matrix and makes it available in all environments
##                   also sets up 4 functions to get/set the matrix & get/set its inverse

## Input matrix, use <<- to make it available globally, initialize its inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y)   {
        x <<- y
        inv <<- NULL
    }
    get <- function() x        ## enable other functions to read the matrix
    getinv <- function() inv   ## enable other fucntions to retrieve its inverse
    setinv <- function (x1) inv <<- x1  ##hopefully only used by cacheSolve and not directly!
    list (set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve:  solves for the inverse of the matrix x
##              If the matrix inverse has already been solved, simply retrieves it

cacheSolve <- function(x, ...) {
        ## See if our work is already done and just return that
    inv <- x$getinv()
    if (!is.null(inv))  {
        message ("getting pre-solved inverse")
        return(inv)
    }
    ## If we get to this point then the inverse must be calculated (assumed invertible)
    data <- x$get()
    inv <- solve (data,...)
    ## Globally set the inverse matrix using <<-
    x$setinv(inv)
    inv  ## return the inverse matrix
}
