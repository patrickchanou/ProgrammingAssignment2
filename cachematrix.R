## The functions below are intended to allow matrix inversion


## The makeCacheMatrix function generates a complex a matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {

my_inv <- NULL
        my_set <- function(b) {
                a <<- b
                my_inv <<- NULL
        }
        get <- function() a
        my_setInverse <- function(inverse) my_inv <<- inverse
        my_getInverse <- function() my_inv
        list(my_set = my_set,
             my_get = my_get,
             setInverse = my_setInverse,
             getInverse = my_getInverse)


}


## The cacheSolve function computes the inverse of that complex "matrix" created by the makeCacheMatrix function above and 
## returns the inverse if the inverse was calculated while the matrix stays unchamged

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
                my_inv <- a$getInverse()
        if (!is.null(my_inv)) {
                message("my cached data")
                return(my_inv)
        }
        my_mat <- a$get()
        my_inv <- solve(my_mat, ...)
        a$setInverse(my_inv)
        my_inv
}
