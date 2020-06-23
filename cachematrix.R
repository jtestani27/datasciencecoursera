## cache the inverse of the matrix
## This saves time from redoing the computation

## Creates a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the inverse of the special "matrix" object above
## if already calculated and not changed, returns cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setInv(inv)
        inv
}