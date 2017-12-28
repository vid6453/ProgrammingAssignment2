## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function takes the matrix object and cache it using set_inverse. get_inverse function holds the inverse of a matrix. 
## So to checked for the inverse matrix which is cached x$get_inverse function is used in cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
				   matrix_inverse <- NULL
				   set <- function(y) {
						   x <<- y
						   matrix_inverse <<- NULL 
						   }
				   get <- function() x	
				   ## sets the inverse of a matrix
				   set_inverse <- function(inv_matrix) matrix_inverse <<-inv_matrix 
				   ## Get the inverse of the matrix
				   get_inverse <- function()  matrix_inverse       
				   list(set = set, get = get, 
						set_inverse = set_inverse,
						get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function retrieves the inverse from the cache.
## This function checks if the inverse is cached, if its cached it will return the cached matrix. 
## If inverse is not found then it computes its inverse and  sets the inverse in cache, and finally returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			matrix_inverse <- x$get_inverse()
	    ## If not matrix inverse is null, it will return the cached matrix.
			if(!is.null(matrix_inverse)) {
				message("getting cached data")
				return(matrix_inverse)
			}
		## if its null, computes its inverse, sets the inverse of the matrix in the cache via the set_inverse function. 
		## Finally returns the inverse of the matrix.
			matrix <- x$get()
			matrix_inverse <- solve(matrix)
			x$set_inverse(matrix_inverse)
			matrix_inverse
}
