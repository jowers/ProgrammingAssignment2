## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setMatrixInverse <- function(matrixInverse) {
		m <<- matrixInverse
	}
	getMatrixInverse <- function() {
		m
	}
	list(set = set, get = get, 
		setMatrixInverse = setMatrixInverse, 
		getMatrixInverse = getMatrixInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getMatrixInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setMatrixInverse(m)
	m
}
