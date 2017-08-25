################################################################################
# Caching the Inverse of a Matrixless 
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
#
# Example of how to use:
#
# > mcm <- makeCacheMatrix(matrix(c(-5, -7, 3, 4), nrow = 2, ncol = 2))
# > cacheSolve(mcm)
#        [,1] [,2]
#   [1,]    4   -3
#   [2,]    7   -5
# > cacheSolve(mcm)
#   Getting cached data
#        [,1] [,2]
#   [1,]    4   -3
#   [2,]    7   -5

# Creates a special "matrix" object that can cache its inverse
# It is assume that the matrix is invertible
#
makeCacheMatrix <- function(x = matrix()) {
    # x has to be a square matrix
    #
    if (class(x) != "matrix" || nrow(x) != ncol(x)) {
        message("First argument has to be a square matrix")
        return(NA)
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then this function should retrieve the inverse from the cache
#
cacheSolve <- function(x, ...) {
    # x has to be a list
    #
    if (class(x) != "list" || length(x) != 4) {
        message("First argument has to be a 4 elements list")
        return(NA)
    }
    
    # Check is the inverse of 'x' is already in the cache
    #
    m <- x$getinv()
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }

    # Calculate the inverse of 'x' and save it to the cache
    #
    data <- x$get()
    m <- solve(data, ...)
    message("Saving cached data")
    x$setinv(m)

    m
}
