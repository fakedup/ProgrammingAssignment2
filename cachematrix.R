## Functions to set and store a matrix and inverse one.

## This function allows to set and get matrix itself and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        InversedMatrix <- NULL ## Define inverse matrix
        set <- function(new_matrix) {
                x <<- new_matrix ##Store new matrix
                InversedMatrix <<- NULL ##Erase inverse matrix if it was computed before
        }
        get <- function() x
        setSolve <- function(solve) InversedMatrix <<- solve
        getSolve <- function() InversedMatrix
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Function to compute inverse matrix and to cache it in previous function.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}