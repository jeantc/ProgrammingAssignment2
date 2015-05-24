## 1. makeCacheMatrix: This function creates 
## a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(mtx = matrix()) {

mtxinverse <- NULL
## result of inversion stored.

set <- function(x) {
	matrix <<- x;
	mtxinverse <<- NULL;
	## initialize mtxinverse to null.

}

get <- function() return(matrix);
setinv <- function(inv) mtxinverse <<- inv;
## set the inversed matrix.
getinv <- function() return(mtxinverse);
## return the inversed matrix.
return(list(set = set, get = get, setinv = setinv, getinv = getinv))
## set matrix, get set matrix, set inversed matrix, get set inversed matrix.

}

## 2. cacheSolve: This function computes the 
## inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix 
## has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {

mtxinverse <- matrix$getinv()
## get the inversed matrix from matrix object.

if(!is.null(mtxinverse)) {
	message("getting cached data")
	return(mtxinverse)
	## return the inversed matrix.
}

data <- matrix$get()
## get the matrix object.
mtxinverse <- solve(data, ...)
## solve the matrix object.
matrix$setinv(mtxinverse)
## set to the object.
return(mtxinverse)
## return inversed matrix solved.

}