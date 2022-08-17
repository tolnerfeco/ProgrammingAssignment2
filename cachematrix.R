# makeCacheMatrix gets an object of type matrix as input and then stores the
# variable to an object into another environment. The function serves with a 
# list of four elements that provides access to the value and the inverse
# of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




# cacheSolve calculates the inverse of the matrix given by makeCacheMatrix.
# If the inverse is already calculate (stored in "m"), then it invokes it,
# otherwise the inverse is calculated by the function and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}




