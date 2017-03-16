## These functions cache the inverse of a matrix


## makeCacheMatrix creates a special matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	InvMat <- NULL

	set <- function(y) {
		x<<-y
		InvMat<<-NULL
	}

	get <- function() x
	setMat <- function(solve) InvMat <<- solve
	getMat <- function() InvMat

	list(set=set, get=get, setMat=setMat, getMat=getMat)
}


## cacheSolve computes the inverse of the special matrix returned by
## makeCacheMatrix, if the inverse already calculated and the matrix
## did not change then it would retrieve it from the cache

cacheSolve <- function(x, ...) {
      InvMat <- x$getMat()

	if(!is.null(InvMat)){
		message("getting cached data")
		return(InvMat)
	}

	data <- x$get()
	InvMat <- solve(data, ...)
	x$setMat(InvMat)

	InvMat
}
