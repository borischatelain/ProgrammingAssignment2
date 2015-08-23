## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
	# Setting cache to NULL
    inverseCached <<- NULL
    
	# Setting matrix entry
    matrixEntry <<- x
	# Inversing the matrix and putting it in the cache
    inverseCached <<- cacheSolve(x)
	
	# Returning the matrix
    x    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'

	# Check if matrix to be inversed is the same that was cached in the past
    if(isTRUE(all(matrixEntry == x))) {

		# If same matrix, then check if cache is null or not to retreive inverse if not null
        if(!is.null(inverseCached)) {
            message("getting cached inversed matrix")
            return(inverseCached)
        }
    }
    
    # if not same matrix or not cached, then return the result of matrix inversion
	# and cache the result for next usage if needed
    message("getting the else")
    inverseCached <<- solve(x)
    inverseCached
}