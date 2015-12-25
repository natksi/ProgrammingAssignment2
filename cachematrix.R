## Setting four functions that store matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
	 set <- function(y) {
	 	x <<- y
	 	i <<- NULL
	 }
	get <- function() x
	setinverse <- function(inverse) i << inverse
	getinverse <- function() i
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Gets inverse of matrix, if matrix not previously calculated, calculates inverse

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)){
			message('getting cacheddata')
			return(i)
	} ## if inverse previously calculate, returns cached data. Else...
	data <- x$get
	i <- solve(data, ...)
	x$setinverse (i)
	i ## return a matrix that is the inverse of 'x'
} 
