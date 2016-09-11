# cachematrix.R contains a pair of functions that cache the inverse of a matrix.

# Usage:
#
# source("cachematrix.R")
# my_mat <- makeCacheMatrix()
# my_mat$set(matrix(1:4, 2, 2))
# my_mat$get()
# cacheSolve(my_mat)


# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {

    # Initialize the inverted matrix variable to NULL
    mat_inv <- NULL

    # Set original matrix to mat variable
    # and reset inverted matrix to NULL
    set <- function(y) {
        mat <<- y
        mat_inv <<- NULL
    }
    
    # Get the original matrix
    get <- function() mat
    
    # Sets the inverted matrix
    set_mat_inv <- function(solve) mat_inv <<- solve
    
    # Gets the inverted matrix
    get_mat_inv <- function() mat_inv

    # Returns a list of above functions
    list(set = set, get = get,
         set_mat_inv = set_mat_inv,
         get_mat_inv = get_mat_inv)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of matrix in 'x' objext
    mat_inv <- x$get_mat_inv()
    
    # If inverse matrix is found in cache 
    if(!is.null(mat_inv)) {
        message("Getting cached inverse matrix")
        return(mat_inv)
    }
    
    # if inverse matrix has not been set
    # retrieve original matrix
    data <- x$get()
    
    # Get the inverse matrix
    mat_inv <- solve(data)
    
    # Set inverted matrix
    x$set_mat_inv(mat_inv)

    # Return inverted matrix
    mat_inv
}
