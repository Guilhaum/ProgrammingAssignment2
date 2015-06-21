## Put comments here that give an overall description of what your
## functions do

## These functions will allow to calculate the inverse of a matrix and store it in an object 
## so that this inverse can be return directly without calculation next time.


## Write a short comment describing this function
## The function makeCacheMatrix will take a Matrix and transform it into a list of 4 functions  
## each one allowing to set the matrix, to get the matrix, to set the inverse of the matrix, to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function cacheSolve will take the previous object with 4 functions 
## and look if an inverse matrix has already been calculated and cached.
## If yes, it will just return the inverse matrix with à short message saying that this result was cached
## If not, it will calculate the inverse , store it in the object and return the inverse.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
