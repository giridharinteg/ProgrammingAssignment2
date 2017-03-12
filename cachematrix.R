# These functions demonstrate the ablility to use lexical scoping 
# to cache potentially time-consuming computations.
# For example, taking the mean of a numeric vector is typically a fast operation.
# For repeated computations If the contents of a object are not changing, 
# it may make sense to cache the value of that object so that when we need it
# again, it can be looked up in the cache rather than recomputed. 

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv_mat <- NULL

set <- function(temp) {
    x <<- temp
    inv_mat <<- NULL
    }
get <- function() x

setinv <- function(t) inv_mat <<- t

getinv <- function() inv_mat

list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)

}


# This cacheSolve function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getinv()
    
    if(!is.null(inv_mat)){
        print("Getting cached data no computation")
        return(inv_mat)
    }
    
    inv_mat <- solve(x$get(), ...)
    x$setinv(inv_mat)
    print("The uncached version")
    inv_mat
}
