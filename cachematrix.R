## This function is meant to solve the problem of caching the inverse of a
## matrix. This function computes the inverse of one matrix and consequently
## stores it in memory where it can be accessed by the user if he needs it again

## This function makes the initial list that contains the necessary functions to
## compute and store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve(x)
	getinverse <- function() inv
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function then either returns the value of the stored inverse of the matrix
## or computes it

cacheSolve <- function(x, ...) {
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
