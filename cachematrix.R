## Cache the input matrix and it's inverse matrix computed.
## If cached before, get the inverse matrix from the cache,
## if not, solve the inverse and make cache.


## cache the matrix and compute the inverse matrix of itself.

makeCacheMatrix <- function(x = matrix()) {
    inX <- NULL
    
    set <- function(y) {
        x <<- y
        inX <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(solve) inX <<- solve
    
    getInv <- function() inX
    
    list( set = set, get = get, setInv = setInv, getInv = getInv )
}


## Get the inverse matrix from the cache.
## If not existing in the cache, compute it by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    inX <- x$getInv()

    if ( !is.null(inX) ) {
        message("getting cache data")
        return(inX)
    }
    data <- x$get()
    inX <- solve(data)
    x$setInv(inX)
    inX
}
