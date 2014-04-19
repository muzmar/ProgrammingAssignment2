## Computing inverse of a matrix is very time consuming
## Thus, once we compute that we will save it for further
## use. These two functions compute the inverse of a 
## matrix and save it and recall it for further use.

## This function creates special matrix that gets and sets
## a matrix and gets and sets its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## define inverse of a matrix as a empty matrix
    inv_x <- matrix()
    
    ## define four functions for get and set
    ## the matrix and its inverse
    set <- function(y) {
        x <<- y
        inv_x <<- matrix()
    }
    get <- function() x
    setinv <- function(inv) inv_x <- inv 
    getinv <- function() inv_x
    
    ## output a special matrix as a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinv()
    data <- x$get()
    
    ## check the matrix and the inverse have the same dimension
    ## and check their product equals to identity matrix or not
    if identical(dim(data), dim(inv_x) & (sum(diag(data %*% inv_x) == 1) == nrow(data)){
        message("getting cached data")
        return(inv_x)
    }
    
    ## when the inverse of matrix is not available, computes and saves it
    inv_x <- solve(data)
    x$setinv(inv_x)
    inv_x
    
}
