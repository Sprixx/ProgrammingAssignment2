## For this assignments two functions are constructed which together
## facilitate the possibility to cache the inverse of a matrix.
## This has the potential to save calculation effort in the case
## of an already calculated inverse.

## The makeCacheMatrix function constructs a special matrix object which 
## is capable of caching an inverse.
## This function constructs 4 functions, set, get, setinverse and getinverse.
## The functions from this object can be used to get and set the matrix
## and to get and set the inverse of an inputted matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special matrix
## inputted by makeCacheMatrix. It sets i (inverse) equal to the inverse of 
## the matrix inputted in makeCacheMatrix by using the getinverse()
## function from makeCacheMatrix. Next, it is checked if this succeeded.
## If this succeeded the message is returned that the cached data is used and 
## the inverse is returned. If this fails the matrix is collected by using
## the get function from makeCacheMatrix. Next i is set equal to the inverse
## of the collected matrix (data) by using the solve() function.
## Next the inverse is set to be i (inverse) using the setinverse function 
## of makeCacheMatrix.
## The next time the function is run again, the getinverse() function will
## succeed and the inverse will not be recomputed, saving computation power.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

