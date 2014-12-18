# "Thu Dec 18 12:16:53 2014" by Dave Robinson
## The following routines create or accept a square Matrix Object
## The matrix object must also be invertible. The makeCache Marix function also
## attaches functions to the object which will solve the matrix when called.
## The second function is the solver function which accepts the matrix object
## and calculates a solution. If the same matrix has already been solved 
## it retrieves the cached solution


#
# makeCacheMatrix creates the objects used by cachesolve including the 
# solver functions. It requires a square invertible matrix to be supplied,
# or the definition of a matrix to be inserted as the first parameter.
# see matrix() in help for matrix definition

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


# cacheSOlve takes as input the object created by the makeCacheMatrix function.
# for example "mcm <- makeCacheMatrix(mat1)" -- where mcm is the new object
# if the matrix has previously been solved it returns the cached answer.
# A usage example is included below as a comment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    
}


# Extra Comments not required by assignment -- memory aid
# for future usage
# Tested on: "Thu Dec 18 12:28:14 2014"

# Usage example
# Test the functions by creating a simple easily verified matrix
# mat1 <- matrix(c(1,2,3,4),2,2) # uncomment to test
# mat1
# This solve(mat1) function should return the same result as the 
# cached function call below -- for validation only!
# solve(mat1) # uncomment to test

# mcm <- makeCacheMatrix(mat1) # creates matrix object from simple matrix
# csm <- cacheSolve(mcm) #" This should produce the same result as solve
# csm  # returns solved object
# mcm$getinv() # invokes "getter" directly on the matrix object

# running cacheSolve(mcm) a second time should retrieve 
# the cached answer and print "getting cached data" in addition to the solution
# cacheSolve(mcm) # Uncomment to run a second time
