##
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
##

##
## This function creates a special "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {

	# Initialised the stored matrix inverse
	m <- NULL

	# Store the matrix object
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# Return the stored matrix object
	get <- function() {
		x
	}

	# Store the inverse matrix
	setMatrixInverse <- function(matrixInverse) {
		m <<- matrixInverse
	}

	# Return the inverse matrix
	getMatrixInverse <- function() {
		m
	}

	list(set = set, get = get, 
		setMatrixInverse = setMatrixInverse, 
		getMatrixInverse = getMatrixInverse)

}


##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	# See if getMatrixInverse already has a stored inverse
	invMatrix <- x$getMatrixInverse()

	if(!is.null(invMatrix)) {
		# getMatrixInverse already had an inverse matrix. Return
		# that and also show a message confirming it came from
		# the cache.
		message("getting cached data")
		return(invMatrix)
	}

	# There was no cached inverse available. Get the matrix object from
	# makeCacheMatrix
	data <- x$get()

	# Calculate the inverse and store it in makeCacheMatrix
	invMatrix <- solve(data)
	x$setMatrixInverse(invMatrix)

	# Return the inverse matrix
	invMatrix
}

