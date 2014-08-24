## Functions together are used to return the inverse of a matrix either through
## fresh computation or from cache (if computed earlier)

## Following function takes a matrix input and creates a list of functions to  
## be used by second function such as caching and returning inverse. Each object
## is created in a new environment and hence is not destroyed ie. remains in
## cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL               ## floating variable; stores inverse; reset to 
                                  ## NULL every time new makeCacheMatrix is 
                                  ## executed
## Next 4 functions are not executed when makeCacheMatrix is called. These are 
## called by cacheSolve to set and return the matrix or its inverse 
        set <- function(y=matrix()){       ## Redundant function
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(z) inv <<- z    ## Sets inverse to value computed by
                                           ## cachesolve function
        getinv <- function() inv   
        list(set = set, get = get,     ## Returns object containing list of 
             setinv = setinv,          ## functions and can be accessed 
             getinv = getinv)          ## externally using $.
## A separate object is created for each x within an independent environment
## which allows caching of inverse of multiple matrices
}

## Following function is used to compute the inverse of a matrix returned by
## makeCacheMatrix. If already computed, retrieves inverse from cache.  

cacheSolve <- function(x) {
        inv <- x$getinv()          ## Return a matrix that is the inverse of 'x'
        if(!is.null(inv)){                    ## If inverse was computed earlier 
                message("Getting cached data")## return inverse from cache and 
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)       ## Compute inverse of matrix
        x$setinv(inv)                ## Set inverse to non NULL. Used as a
        inv                          ## condition in "if" above.
}
