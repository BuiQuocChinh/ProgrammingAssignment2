## Matrix inversion is usually a costly computation. To avoid this, we create a funtion that caches the inverse 
## of a matrix rather than compute it repeatedly. Below are two functions that are used to create a special object that ## stores a matrix and cache's its inversion

## This function creates a special "matrix" that peforms the following operation:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion of the matrix
## 4. get the value of the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function. 
## However, it first checks to see if the inversion has already been calculated. 
## If so, it gets the inversion from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
