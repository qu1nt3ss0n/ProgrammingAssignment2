## To Cache the inverse of a matrix - Matrix inversion is usually a costly computation 
## and there may be benefits to caching the inverse of a matrix rather than computing it repeatedly. 
## Below is a pair of functions that creates a special matrix object (with the help of get & set operators),
## caches its inverse. It then retrieves & either recomputes or returns that cache.  

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## sets the value of the matrix - set
## gets the value of the matrix - get
## sets the inverse of the matrix - setinverse
## gets the inverse of the matrix - getinverse

makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL
        set <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Calculates the inverse of the special matrix returned by makeCacheMatrix (setinverse).  
## However, if the inverse has already been calculated (i.e. the matrix has not changed), then it simply retrieves the 
## inverse from the cache (getinverse).

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) 
        {
                message("retrieving cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
