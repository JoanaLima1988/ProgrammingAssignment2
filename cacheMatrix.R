makeCacheMatrix <- function(M = matrix()) {
        I <- NULL
        set <- function(y) {
                M <<- y
                I <<- NULL
        }
        get <- function() M
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(M, ...) {
        I <- M$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- M$get()
        I <- solve(data, ...)
        M$setinverse(I)
        I
}


