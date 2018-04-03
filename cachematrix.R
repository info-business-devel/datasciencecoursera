
# cachematrix.R

## Function: cacheSolve <- function(x, ...)

### 'x' is a matrix.
### This function asks function makeCacheMatrix whether or not it has 'x' in cache.
### If makeCacheMatrix has 'x' in cache that means there is no need to create its inverse matrix 
### because this is already in cache (both are in cache: 'x' and its inverse).

### If 'x' is not in cache, the function cacheSolve has to create its inverse and call
### function makeCacheMatrix to put both of them ('x' and its inverse) in cache.

### The regular operation of this function is (example):
### matrix_1 <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2)
### cacheSolve(makeCacheMatrix(matrix_1))
### matrix_1 is not in cache.
### cacheSolve asks makeCacheMatrix if matrix_1 is in cache (not, it isn't).
### cacheSolve creates its inverse and call makeCacheMatrix to put both in cache.
### Now matrix_1 and its inverse are both in cache.

### matrix_2 <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2)
### cacheSolve(makeCacheMatrix(matrix_2))
### matrix_2 is identical to matrix_1.
### cacheSolve asks makeCacheMatrix if matrix_2 is in cache (yes, it's).
### There is no need to create matrix_2 inverse just retrieve it from cache.
### cacheSolve calls makeCacheMatrix and retrieves matrix_1 inverse.
### cacheSolve returns matrix_1 inverse (identical to matrix_2 inverse).


## Function: makeCacheMatrix <- function(x = matrix())

### This function just waits to be called by function cacheSolve.
### It has got 5 'methods':

#### get_data: It returns 'x', the argument of the function (in previous example: matrix_1 or matrix_2).
#### set_initial_matrix: It puts the initial matrix in cache.
#### get_initial_matrix: It gets the initial matrix from cache.
#### set_inverse_matrix: It puts the inverse matrix in cache.
#### get_inverse_matrix: It gets the inverse matrix from cache.


makeCacheMatrix <- function(x = matrix()) {
        
        # This function puts and gets matrices in/from cache
        
        get_data <- function() { 
                return(x)  # It just returns the matrix argument of the function
        }
        set_initial_matrix <- function(matrix_temp) {
                initial_matrix <<- matrix_temp  # It puts the initial matrix in cache
        }
        get_initial_matrix <- function() {
                return(initial_matrix)  # It gets the initial matrix from cache
        }
        set_inverse_matrix <- function(matrix_temp) {
                inverse_matrix <<- matrix_temp  # It puts the inverse matrix in cache
        }
        get_inverse_matrix <- function() {
                return(inverse_matrix)  # It gets the inverse matrix from cache
        }
        list(
                get_data = get_data,
                set_initial_matrix = set_initial_matrix, 
                get_initial_matrix = get_initial_matrix,
                set_inverse_matrix = set_inverse_matrix,
                get_inverse_matrix = get_inverse_matrix)
}

cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x'
        # If 'x' is in cache, it just gets its inverse by calling makeCacheMatrix
        # If 'x' is not in cache, it creates its inverse and caches both by calling makeCacheMatrix
        
        # It gets the initial matrix if cache (if there is any)
        initial_matrix_in_cache <- x$get_initial_matrix()
        
        # It gets the 'x' matrix in the function by calling makeCacheMatrix
        x_matrix <- x$get_data()
        
        if(!is.null(initial_matrix_in_cache))
        {
                if (identical(initial_matrix_in_cache,  x_matrix)) {
                        
                        # There was an identical matrix in cache and returns the inverse from cache
                        message("The matrix is in cache. Getting cached inverse matrix.")
                        return(x$get_inverse_matrix())                         
                }
        }
        
        # There wasn't an identical matrix in cache so it has to create its inverse
        inverse_matrix <- solve(x_matrix, ...)
        
        # It puts the initial matrix in cache
        x$set_initial_matrix(x_matrix)
        
        # It puts the inverse matrix in cache
        x$set_inverse_matrix(inverse_matrix)
        
        # It returns the inverse of 'x'
        return(inverse_matrix)
}

# Firs declaration of initial_matrix ('x' matriz in cache)
initial_matrix <- NULL

# Try it by removing '#' before the following lines:
## matrix_1 <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2)
## cacheSolve(makeCacheMatrix(matrix_1))
## matrix_2 <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2)
## cacheSolve(makeCacheMatrix(matrix_2))

