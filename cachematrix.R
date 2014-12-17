## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

mat1 <- matrix(c(1,2,3,4),2,2)
mat1
solve(mat1)

mcm <- makeCacheMatrix(mat1)

cacheSolve(mcm)


mat1 <- matrix(c(1,0,0,1),2,2)
mat1
solve(mat1)

mat1 <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
mat1
solve(mat1)

mat1

mat1 <- matrix(rnorm(1:49, mean = 10, sd=2),7,7)
solve(mat1)
mat1

mat1 <- vec2diag(matrix(1:4, 1, 4))
mat1 <- diag(3)
solve(mat1)
mat1

