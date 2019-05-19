## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#=================================
#
# Caching the Inverse of a Matrix
#
#=================================

## The makeCacheMatrix function creates a matrix object and returns a set of functions 
## --get(), set(), setinverse(), get(inverse) within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        # initialize x to an empty matrix, and inv to NULL
        inv <- NULL
        
        # assign input argument to x and clears the value of inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # retrieve x from the parent environment
        get <- function() x
        
        # define the setter and getter for matrix inverse
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        
        # return a list containing above functions with named elements
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

## The cacheSolve function retrieves the inverse matrix from cache value stored
## in parent environment. If cache is empty, then computes and prints out matrix
## inverse given input argument x.

cacheSolve <- function(x, ...) {
        # retrieve a matrix that is the inverse of x
        inv <- x$getinverse()
        
        # first check if there is a valid cached mean
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # if NULL, get input argument, compute and return inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}