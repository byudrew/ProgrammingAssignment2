## The cacheSolve and makeCacheMatrix functions allow a user to
## create a matrix and store its inverse without having to compute
## the inverse more than once. The inverse is only recomputed when
## the matrix changes.


## makeCacheMatrix returns a list of functions to set and get
## a local variable (mat) and its inverse (inv)

makeCacheMatrix <- function(mat = matrix()) {
    
    # initialize inverse as NULL
    inv <- NULL
    
    # change matrix and set inverse to NULL
    setmat <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    
    # get matrix
    getmat <- function() mat
    
    # set a value for inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get inverse
    getinv <- function() inv
    
    # return a list of the 4 get/set functions
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve returns the inverse of a matrix. Its input is the
## list of functions created by makeCacheMatrix. If the inverse
## of the original matrix has already been computed, that value
## is returned. If not, it will be computed, cached, and returned.

cacheSolve <- function(x, ...) {
    
    # get the inverse
    inv <- x$getinv()
    
    # if the inverse has been cached return the cached value
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # get the saved matrix in x
    data <- x$getmat()
    
    # compute the inverse of that matrix
    inv <- solve(data, ...)
    
    # cache the inverse
    x$setinv(inv)
    
    # return the inverse
    inv

}
