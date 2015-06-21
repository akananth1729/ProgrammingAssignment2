## These functions allow the user to compute and cache the inverse of a square matrix


##  This function creates a special type of square matrices -- Cache Matrix -- that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        if(length(dim(x)) != 2 || dim(x)[1] != dim(x)[2])
                stop("x must be a square matrix")

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the "Cache Matrix" object defined by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache. Otherwise, it computes the inverse and caches it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
