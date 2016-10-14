## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               # assign the cache to m
        set <- function(y) {    # this function assignes y to x
                                # and the cache m to NULL in the parent enviroment
                x <<- y
                m <<- NULL
        }
        get <- function() x     # assignes the getter with x form the parent enviroment
        set_inverse_matrix <- function(inverse) m <<- inverse # assignes
        get_inverse_matrix <- function() m     # defines the getter for the inverse Matrix
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix) #makes a list with the function we set and name them
}


## Returns the inverse matrix of the matrix "x" from the cache. If the is no value in the cache it calcualtes the inverese matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse_matrix()     # assign the inverse matrix to m
        if(!is.null(m)) {               # if the m is not NULL, prints massage "getting cache data"
                message("getting cached data")
                return(m)               # and returns m
        }
        data <- x$get()                 # else assign the getter from above function to data
        m <- solve(data, ...)           # and recomputes the inverse matrix of "x"
        x$set_inverse_matrix(m)         # sets m in the cache
        m                               # prints  m
}

