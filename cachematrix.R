# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix eturns a list of functions that do as follows:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # create inv to store the inverse of the cached matrix
        inv <- NULL
        
        # set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the matrix
        get <- function() x
        
        # set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # get the inverse
        getinverse <- function() inv
        
        # return matrics with above defined functions
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: computes the inverse. Returns the cached inverse if it has already been calculated.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If inverse already calc'ed, return it
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        # If the invers is not calced already, we will calc it      
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Test values
## > x <- rbind(c(-76, 8), c(8, -76))   // matrix values
## > m <- makeCacheMatrix(x)            // create the special matrix
## > m$get()                            // return the matrix
## [,1] [,2]
## [1,]  -76    8
## [2,]    8  -76

## first run (no cache)
## > cacheSolve(m)
## [,1]        [,2]
## [1,] -0.01330532 -0.00140056
## [2,] -0.00140056 -0.01330532

## second run retrieving the cache
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]
## [1,] -0.01330532 -0.00140056
## [2,] -0.00140056 -0.01330532