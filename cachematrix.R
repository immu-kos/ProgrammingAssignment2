## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix

## These function are made ass the example for catching the mean of a vector, 
## but computing the invese of a matrix instead.
## Exept name-changes of variables, the major change vas to use the 
## solve() function instead of the mean()

## The first function, inverse_matrix_cache creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}
    
## The following function calculates the mean of the special "matrix" created 
## with the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix, the inverse of x
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}


