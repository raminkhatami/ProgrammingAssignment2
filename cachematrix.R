## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This Function is a class like object that returns a list.
makeCacheMatrix <- function(x = matrix()) {
	#' cach a matrix and its inverse
    	#'
    	#' @param x A `matrix`
	#' @return a `list` that contains set,get, setmean, getmean methods
	#' @usage makeCacheMatrix(matrix(rnorm(9),3))
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverted <<- inv
        getinverse <- function() inverted
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# returns the invert of a matrix 
cacheSolve <- function(x, ...) {
        #' Return a `matrix` that is the inverse of 'x'
	#' However, it first checks to see if the inverse  has already been 
	#' calculated. If so, it gets the inverse from the cache and skips the computation. 
	#' Otherwise, it calculates the inverse of the x and sets the value of 
	#' the inverse  in the cache via the setinverse function.
	#'
	#' @param x A makeCacheMatrix object
        #' @return inverse of x
        #' @usage  t=makeCacheMatrix(matrix(rnorm(9),3)); cacheSolve(t);
        inverted <- x$getinverse()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setinverse(inverted)
        inverted
}
