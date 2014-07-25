## To calculate the inverse of a matrix in a faster way,
## this function firstly looks up in the cache before computation.

## makeCacheMatrix contains a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set (specifically, solve) the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <<- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix
## created with the above function. It first checks to see
## if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets
## the value of inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix 'i' that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}