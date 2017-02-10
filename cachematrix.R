##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ##initialize object
    m <- NULL
    ##whenever the input value changes clear the cached value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ##retrive the value of x from parent environement
    get <- function() x
    ##set the value of inverse matrix
    setinverse <- function(inverse) m <<- inverse
    ##retrieve value from parent environement
    getinverse <- function() m
    ##set names of the 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ##return value of inverse matrix
    m <- x$getinverse()
    ##if value set from previous call return value from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ##get inverse matrix by calling solve function
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
