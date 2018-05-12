#this function gets the value of the inverse of the matrix M and puts it into a matrix I.


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

#this function checks if the inverse of the matrix was calculated. If so, it returns the inverse of the matrix. If not, it calculates it 
#and returns it.

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


