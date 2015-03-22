## These two functions cache a matrix (1) and its inverse (2), 
## to save computation time if the operation has to be repeated.


## makeCacheMatrix uses a matrix as argument, and returns a list.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve uses the list returned by makeCacheMatrix as argument. 
## It calculates the inverse matrix (using solve function) or retrieves it, 
## if it has been calculated before and the argument was not altered 
## (in this case, it will print a message indicating that.) 

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
