## These functions cache the inverse of a square matrix, upon first 
## calculation of the inverse, so that if the inverse is desired many 
## times, we do not need to spend the memory every time.

## The makeCacheMatrix function creates a list of four functions, 
## associated with a matrix, that set and return the matrix and its 
## inverse.

makeCacheMatrix <- function(A = matrix()){
     Ainv <- NULL
     set <- function(B){
          A <<- B
          Ainv <<- NULL
     }
     get <- function() A
     setinv <- function(Binv) Ainv <<- Binv
     getinv <- function() Ainv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolv function is applied to a list of functions created 
## by makeCacheMatrix.  It returns the inverse, if already cached, or 
## else computes it, returns it, and caches it

cacheSolv <- function(x, ...) {
     Ainv<- x$getinv()
     if(!is.null(Ainv)) {
          message("getting cached data")
          return(Ainv)    
     }
     data<-x$get()
     Ainv<-solve(data, ...)
     x$setinv(Ainv)
     Ainv
        ## Return a matrix that is the inverse of 'x'
}
