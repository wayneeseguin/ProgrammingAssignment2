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
    # Define CacheMatrix Function to set the object and inverse cache
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # Define CacheMatrix function to set the inverse in the cache
    setInverse <- function(inv) inverse <<- inv

    # Define CacheMatrix function to get the cached inverse
    getInverse <- function(inverse)

    # Define CacheMatrix function to tell if a cached inverse exists
    isCached <- function() !is.null(inverse)

    # Cache the given matrix object
    set(x)

    # Return the list of defined functions on the new CacheMatrix object
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         isCached = isCached
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
    # If the computation is not cached,
    if(!x$isCached()) {
        # Perform the computation
        inv <- solve(x$get(),...)

        # and set the results to the CacheMatrix's cache.
        x$setInverse()
    }

    # Return the inverse from the cache.
    x$getInverse()
}
