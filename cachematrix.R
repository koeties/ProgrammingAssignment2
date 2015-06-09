## The following functions will read in a matrix and compute it's inverse.  It will also cache the inverse for future requests

## The makeCacheMatrix function creates a list containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function calculates the inverse of the matrix and retrieves the cached instance of the matrix's inverse if available
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached inverse of matrix.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

# To test the functions, run the below in the R console:
# "test" is a matrix that is reversible
#
# test <- matrix(c(0.5,0.5,0.25,0.25,0,0.25,0.25,0.5,0.5),3,3)
# m_func <- makeCacheMatrix(test)
# m_func$get()
# cacheSolve(m_func)