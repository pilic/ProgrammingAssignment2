## makeCacheMatrix: creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
    in_x <- NULL
    set <- function(y) {
        x <<- y
        in_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) in_x <<-inverse
    getinverse <- function() in_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: calculates the inverse of the matrix 
cacheSolve <- function(x, ...) {
    in_x <- x$getinverse()
    if (!is.null(in_x)) {
        return(in_x)
    } 
    m <- x$get()
    in_x <- solve(m, ...)
    x$setinverse(in_x)
    in_x
}
