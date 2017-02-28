# Programming Assignment 2


# This function creates a special matrix object that can cache its inverse
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Test Case 1:
# 
# a <- diag(5,5)
# m <- makeCacheMatrix(a)
# cacheSolve(m)
# cacheSolve(m)
# 
# Test Case 2:
# 
# r = rnorm(1000000)
# m1 = matrix(r, nrow=1000, ncol=1000)
# m = makeCacheMatrix(m1)
# cacheSolve(m)
# cacheSolve(m)