##
## Function: makeCacheMatrix
## 
## Creates a special CacheMatrix object which can store the results of a
## matrix inverse calculation so it only has to be computed once.
## 
## Parameters: A Matrix
##
## Returns: a list of functions defined on the given CacheMatrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Define CacheMatrix Function to set the object and cache
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    # Define CacheMatrix Function to get the Matrix
    get <- function() x

    # Define CacheMatrix function to set the cache
    setCache <- function(inv) cache <<- inv

    # Define CacheMatrix function to get the cache
    getCache <- function() cache

    # Cache the given matrix object
    set(x)

    # Return the list of defined functions on the new CacheMatrix object
    list(set = set,
         get = get,
         setCache = setCache,
         getCache = getCache
         )
}

##
## Function: cacheSolve
##
## Computes the inverse of a matrix and stores it in the special 
## CacheMatrix's cache, if it has not already been computed 
##
## Parameters: An (assumed) Invertable Matrix.
##
## Returns: A matrix which is the inverse of the original
##
cacheSolve <- function(x, ...) {
    # Fetch the cache value
    result <- x$getCache()

    # If the computation is not cached,
    if(is.null(result)) {
        # Perform the computation
        result <- solve(x$get(),...)

        # and set the results to the CacheMatrix's cache.
        x$setCache(result)
    }

    # Return the cached inverse
    result
}
