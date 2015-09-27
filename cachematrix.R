# This function does the inverse matrix optimizing the processing 
# Obs: This assumes that the matrix is always invertible

# makeCacheMatrix is a function (like a class) that creates a list containing
# functions to:
#  A - SET the value of the matrix
#  B - GET the value of the matrix
#  C - SET the value of inverse of the matrix
#  D - GET the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, 
             getinverse=getinverse)
}

# This function returns the inverse of the matrix doing:
# A - Checks if the inverse has already been computed.
# B - If YES, gets the cached result and skips the computation.
# C - If NOT, computes the inverse, sets the value in the cache via
#     setinverse function (above).


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        # Note: solve function does the inverse of the matrix (square)
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## To test this function RUN:

# x = rbind(c(10, -1/3), c(1/4, 25))
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
# cacheSolve(m)

## Note that in the first run there was no cache, but in the second run (and 
## the next ones) the function got the cached solution
