##The role of the functions is to create a matrix for which we can cache the inverse, it is a very
##computationally heavy process.

## MakeCacheMatrix creates a list of functions and matrix for which we can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        #first initiate
        inverse <- NULL 
        #define get function will be called by cachemean
        get <- function() x
        #define the get function for inverse
        getinverse <- function() inverse
        #caching inverse
        setinverse <- function(y) inverse <<- y
        list(get=get,getinverse=getinverse,setinverse=setinverse)
}


# Cache solve return the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Fetch the inverse
        inverse <- x$getinverse()
        ## Testing if it was cached or not
        
        if (!is.null(inverse)) {
                ## Use the inverse that was cached
                message("getting cached inverse")
                return(inverse)
        } else {
                ## load the matrix and inverse it
                matrix <- x$get()
                inverse <- solve(matrix)
                ## cache the inverse
                x$setinverse(inverse)
                return(inverse)
        }  
        
}

