## Create a CacheMatrix 'object' with several functions and a variable
## to store the matrix' inverse
makeCacheMatrix <- function(x = matrix()) {
	## Initialize values
	x <- x
	inv <- NULL
	## set(y): Redefine the object
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## get(): Retrieve the original data
	get <- function() x
	## setinv(i): Set the inverse for the matrix
	setinv <- function(i) inv <<- i
	## getinv(): Retrieve the inverse (returns NULL if undefined)
	getinv <- function() inv
	
	## Return the 'object' as a list
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}

## This function takes as input a CacheMatrix 'object' and returns its inverse.
## If the inverse was calculated before, it will be retrieved from the cache.
## If not, it will be calulated and stored.
cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	## Check whether the result was cached
	if(!is.null(inv)) {
		## if so, return it
		message("getting cached data")
		return(inv)
	}
	## If the inverse was not cached, calculate it and store it in x
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	## Return the inverse
	inv
}
