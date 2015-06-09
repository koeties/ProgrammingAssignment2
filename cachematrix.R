## The following functions will read in a matrix and compute it's inverse.  It will also cache the inverse for future requests

## The makeCacheMatrix function creates a list containing functions to set and get a matrix as well as to set and get it's inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	# Define the set function for setting the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# Define the get function to get the matrix
	get <- function(){x}
	# Define the set function to set the matrix's inverse
	setinverse <- function(inverse){inv <<- inverse}
	# Define the get function to get the matrix's inverse
	getinverse <- function() inv
	# Create the named list of functions for the matrix operation
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function calculates the inverse of the matrix and retrieves the cached instance of the matrix's inverse if available
cacheSolve <- function(x, ...) {
	# get the getinverse function of the matrix
	inv <- x$getinverse()
	# look if a cached version of the inverse is available.  If it is, return it
	if(!is.null(inv)) {
		message("getting cached inverse of matrix.")
		return(inv)
	}
	# Cached version not available, let's get the matrix and compute it's inverse
	data <- x$get()
	inv <- solve(data)
	# Call the setinverse function of the matrix to set it's inverse
	x$setinverse(inv)
	# Return the matrix inverse
	inv
}

# To test the functions, run the below in the R console:
# "test" is a matrix that is reversible
#
# test <- matrix(c(0.5,0.5,0.25,0.25,0,0.25,0.25,0.5,0.5),3,3)
# m_func <- makeCacheMatrix(test)
# m_func$get()
# cacheSolve(m_func)