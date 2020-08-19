## Author: Nourhan Ihab
## Date: 20/8/2020
## Course: R Programming
## Offered By: John Hobkins University/Coursera
## Version 1.0

## To Decrease the time consuming computations, We developed 2 functions
## to cache the results to matrix and its inverse. So, when a matrix is
## computed again, the cached answer is shown instead of computing the
## matrix again. One sets the data of the matrix and its result (makeCacheMatrix)
## the other checks if this matrix was solve before or not. If yes it returns the
## the result, else the matrix is solved and added to the chache (cacheSolve).

## Create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        m <- x$getsolve()
        
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
        m
}
