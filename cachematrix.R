## Creates special list using input matrix

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <-  function() x
    setinv <- function(x) cache <<- solve(x)
    getinv <-  function() cache
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates inverse using Special list from above

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    else {
        data <- x$get()
        solve(data)     ## Return a matrix that is the inverse of 'x'
    }
}
