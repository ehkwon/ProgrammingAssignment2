## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 'makeCacheMatrix' creates a special "matrix", which is really a list
## containing a function to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
				m <- NULL ## Initialize to NULL
			
				## '<<-' used to assign a value to an object in an environment
				## that is different from the current environment.
				## Create the matrix in the current environment
				set <- function(y) {
								x <<- y 
								m <<- NULL
				}
				## Get the value of the matrix
			  get <- function() x
			  ## Inverse the matrix
				setMatrix <- function(inverse) m <<- inverse
				## Get the inversed matrix
				getInverse <- function() m
				## Return the created functions
				list(set = set, get = get,
						 setMatrix = setMatrix,
						 getInverse = getInverse) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
				
        ## If the matrix has already been calculated 
				if(!is.null(m)) {
								message("getting cached data")
								return(m)
				}
        ## Get the matrix stored with makeCacheMatrix
        matrix <- x$get()
				m <- solve(matrix, ...)
				## Set inverted matrix 
				x$setMatrix(m)
				return(m)
}
