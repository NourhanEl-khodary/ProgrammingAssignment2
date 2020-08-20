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
        m <- NULL                                    ## Intialize a vriable for the matrix's inverse
        
        set <- function(y) {                         ## function that sets the matrix and its intilaization of it if it changed
                x <<- y
                m <<- NULL
        }
        
        get <- function() x                          ## Function returns the matrix that is being applied on
        
        setsolve <- function(solve) m <<- solve      ## Function sets the inverse of the matrix
        
        getsolve <- function()                       ## Function returns the inverse of the matrix
        
        list(set = set, get = get,                   ## Adding the result to a list to search in if there is multiple results
             setsolve = setsolve,
             getsolve = getsolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        m <- x$getsolve()                      ## Get the inverse of the matrix from makeCacheMatrix function
        
        if(!is.null(m))                        ## Check if the matrix has been computed before
        {
                message("getting cached data") ## If yes the computed result in the cache is returned
                return(m)
        }
        
        data <- x$get()                        ## If not the matrix is returned from makeCacheMatrix
        m <- solve(data, ...)                  ## inverse the matrix
        x$setsolve(m)                          ## set the inversed matrix to the special matrix object
        
        m                                      ## Then return the result which is the inverse of the matrix
}
