## This function creates a special Matrix-Object that can cache its Inverse.
## It supports setting Matrix, getting Matrix, setting Inverse and getting
## Inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invers) inv <<- invers
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special Matrix-Object 
## returned by the function makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the Inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
