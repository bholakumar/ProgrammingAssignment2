## Matrix inversion is usually a costly computation and there is a benefit to 
## use when required, rather compute every time.

## Following two functions (makeCacheMatrix and cacheSolve) are used to 
## create a object that stores a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
    # 1. set the value of the matrix
    # 2. get the value of the matrix
    # 3. set the value of inverse of the matrix
    # 4. get the value of inverse of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y    #  `<<-` assign a value to an object in an environment
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# Function cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
