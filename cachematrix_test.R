source("cachematrix.R")

pass <- function() message("  => pass")
fail <- function() message("  => fail")

message("Creating Matrices...")
Matrix1 <- matrix( c(3,6,1,9) , nrow = 2, ncol = 2)
Matrix2 <- matrix( c(2,3,4,5), nrow = 2, ncol = 2)

message("Computing Correct Inverses...")
Matrix1Inverse <- solve(Matrix1)
Matrix2Inverse <- solve(Matrix2)

message("\nMaking a CacheMatrix from Matrix1...\n")
CacheMatrix <- makeCacheMatrix(Matrix1)

message("CacheMatrix should have a null cache....")
c <- CacheMatrix$getCache()
if(is.null(c)) { pass() } else { fail() }

message("\nCalling cacheSolve(CacheMatrix)\n")
cacheSolve(CacheMatrix)

message("The result should be cached...")
c <- CacheMatrix$getCache()
if(is.null(c)) { fail() } else { pass() }

message("The cached matrix should match Matrix1Inverse...")
c <- CacheMatrix$getCache()
if(identical(c,Matrix1Inverse)) { pass() } else { fail() }

message("\nSetting CacheMatrix to contain Matrix2\n")
CacheMatrix$set(Matrix2)

message("The CacheMatrix should have an empty cache...")
c <- CacheMatrix$getCache()
if(is.null(c)) { pass() } else { fail() }

message("\nCalling cacheSolve(CacheMatrix)\n")
cacheSolve(CacheMatrix)

message("The result should now be cached...")
c <- CacheMatrix$getCache()
if(is.null(c)) { fail() } else { pass() }

message("The cached matrix should match Matrix2Inverse...")
c <- CacheMatrix$getCache()
if(identical(c, Matrix2Inverse)) { pass() } else { fail() }

