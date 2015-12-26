## Setting four functions that store matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
	 set <- function(y) {
	 	x <<- y
	 	i <<- NULL
	 }
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Gets inverse of matrix, if matrix not previously calculated, calculates inverse

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)){
			message('getting cacheddata')
			return(i)
	} ## if inverse previously calculate, returns cached data. Else...
	data <- x$get
	i <- solve(data, ...)
	x$setInverse (i)
	i ## returns a matrix that is the inverse of 'x'
} 
