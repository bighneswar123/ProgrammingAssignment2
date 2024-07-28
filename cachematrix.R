# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

# Example of usage:
# Create a matrix
m <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special matrix object
cachedMatrix <- makeCacheMatrix(m)

# Compute the inverse and cache it
inverse1 <- cacheSolve(cachedMatrix)
print(inverse1)

# Retrieve the cached inverse
inverse2 <- cacheSolve(cachedMatrix)
print(inverse2)
