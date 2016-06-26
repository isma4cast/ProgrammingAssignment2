## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## The following two functions cache the inverse of a matrix.


# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# retrieves the inverse from the cache.
# It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...){
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Sample run:
##> x = rbind(c(3, 1), c(4, 2))
##> x <- rbind(c(3, 1), c(4, 2))
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    3    1
##[2,]    4    2
##> cacheSolve(m)
##[,1] [,2]
##[1,]    1 -0.5
##[2,]   -2  1.5
##> cacheSolve(m)
##getting cached data.
##[,1] [,2]
##[1,]    1 -0.5
##[2,]   -2  1.5