## These functions contain subfunctions to manage a matrix, cache the inverse
## and return the inverse first from the cache (if exists) or calculates the 
## inverse, saves the results to the cache, and returns the inverse value to
## the user.

## Function: makeCacheMatrix
## Description:
## This function creates a special vector which is really a list containing a function to do the following:
## 1. set a matrix
## 2. get a matrix
## 3. set the inverse (using solve) of a matrix
## 4. get the inverse (using solve) of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function: cacheSolve
## Description:
## This function calculates the inverse of the special matrix created with 
## function makeCacheMatrix.  Before calculating the inverse (using solve)
## the function checks to see if the inverse has already been calculated and
## cached for later reference.  If the function finds that the inverse has 
## been cached it returns the cached value, otherwise the function calcs
## the inverse using solve and returns the inverse matrix.
cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	mtx <- x$get()
	inv_x <- solve(mtx, ...)
	x$setsolve(mtx)
	# Return a matrix that is the inverse of 'x'
	inv_x
}
