## Caching the inverse of a matrix (instead of computing it over and over)

## Create an instance of 'makeCacheMatrix' 
## using private members x & m, 
## and public methods set, get, setmatrix, and getmatrix

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}


## Compute teh inverse of the matrix created in the above function
## This will return a matrix that is the inverse of 'x'

cacheSolve <- function(x = matrix(), ...) {

	m<- x$getmatrix()
	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}