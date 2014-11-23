## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix establishes a cache (containing a matrix or the NULL value)
# for storing the computed result, and also establishes a list of functions
# which operate on the cache.
# makeCacheMatrix must be called first to initialise the cache and the list
# of functions.
# The set function is then called to store the matrix to be inverted (in x),
# and to initialise the result cache (m).
# cacheSolve returns the inverted matrix from the cache, if already available,
# or computes it and then sets the cache.
# The fundamental programming model is that of a Closure with non-local
# assignments, as described in 5.4 of Chambers, Software for Data Analysis

## Write a short comment describing this function
# The makeCacheMatrix function is an initialisation function to establish a
# cache m and a list of functions. The list is returned when makeCache Matrix
# is called, and m is preserved until the calling environment is destroyed.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                 # mark cache m as NULL
        set <-function(y) {
                x<<-y           # y is the matrix to be inverted
                m<<- NULL       # mark cache m as NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse       #set inverse stores the
                                                        #inverted matrix in m
        getinverse<-function() m        # return cached inverse matrix from m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The cacheSolve function checks the cache m to see if it already contains a
# valid solved matrix. If so it returns that value, otherwise the solve
# function is called on the given matrix to invert (or otherwise solve) it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse         # set m to the cached inverse matrix
        # The value of m may either be NULL, or an actual inverted matrix
        if(!is.null(m)) {
                # The cache m contained a valid inverted matrix
                message("Getting cached data")
                return (m)      # And return the cached data
        }
        # No valid data in the cache, prepare to invert the matrix
        data<-x$get()           # apply get method to x to set the value of
                                # data (to be solved)
        m<-solve(data, ...)     # invert (or otherwise solve) the matrix data
        x$setinverse(m)         # and store the result in the cache
        m                       # Return the freshly inverted matrix
}
