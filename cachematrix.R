## makeCacheMatrix creates a new matrix object. 
## a <- makeCacheMatrix() creates an empty matrix object.
#########################################################
## m <= matrx(rnorm(9),3,3)
## a <- makeCacheMatrix(m) creates a matrix object m and allows us to use 
## set,get,setinverse and getinverse functions on m

## set sets a to a new matrix object, get returns the matrix, set inverse sets the inverse of a
## get inverse gets the inverse of a. The function returns a list of these four operations

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(t,u = nrow(t),v = ncol(t)) {
		x <<- matrix(t,u,v)
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function gets the inverse of the matrix from cache if cache is available. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached inverse")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
        
}
