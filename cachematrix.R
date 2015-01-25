## Matrix inversion is usually a costly computation. Storing the inverse in cache
## will avoid having to recalculate each time needed.  The below functions
## work together to cache the inverse of a matrix.

## The makeCacheMatrix function is designed to create a matrix object full of
## functions that assit in caching the matrix inverse.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #variable for matrix inversion
    set <- function(y) {   #sets the value of the matrix
            x <<- y  #caches the input matrix
            inv <<- NULL #sets the value of (matrix inverse) inv to Null 
    }
    get <- function() x  #return the matrix that was input
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv  #return the inversed matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachesolve() should return the inverse of the matrix which is passed
## as an argument to the makeCacheMatrix(). 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' by first checking if the 
    ## inverse has already been created. If inverse already created, it is 
    ## pulled from cache. If not already created, it is set here.
    
    inv <- x$getinverse()  #gets inversed matrix from x
    if(!is.null(inv)) {
            message ("inverse pulled from cache")
            return(inv)
        }
    data <- x$get()  #if no inversion is stored, then get the matrix object
    inv <- solve(data)
    x$setinverse(inv)  #sets the inverse to no longer be null
    inv
}
