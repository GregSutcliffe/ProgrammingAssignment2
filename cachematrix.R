## Two functions for caching the solve() of a square matrix.  No checking is
## done to ensure the matrix is invertible, please ensure the matrix can be
## inverted.

## To use these functions, first initialize makeCacheMatrix with a square
## matrix, then call cacheSolve on the resulting object, eg:

# my_matrix <- matrix(c(2,0,0,2),2,2)
# cached_matrix <-makeCacheMatrix(my_matrix)
# cacheSolve(cached_matrix)

## This function uses getters and setters to return an object that can store
## the cached solve result

makeCacheMatrix <- function(x = matrix()) {
    # Variable setup
    # x is a matrix, already initialized from the formal args
    inverse <- NULL # For storing the result of solve()

    # Getters
    get         <- function() x
    get_inverse <- function() inverse

    # Setters
    set <- function(y) {
        x <<- y
        inverse <<- NULL # reset this is we change x
    }
    set_inverse <- function(inv) inverse <<- inv

    # Return object, list of getter/setter methods
    list(get = get,
         get_inverse = get_inverse,
         set = set,
         set_inverse = set_inverse)
}

## This function consumes the makeCacheMatrix object and
## tests to see if the inverse has been cached, returning
## it if so. If not, it is computed, cached, and returned

cacheSolve <- function(x, ...) {
    # Read the current stored inverse
    inverse <- x$get_inverse()

    # If this is not empty, we have a cached value, return it
    if(!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }

    # No cached value, compute solve(), cache it, and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
