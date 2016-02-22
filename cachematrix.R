## makeCacheMatrix and solveCache functions are used in conjuction
## to create and manipulate a set of functions for storing/retrieving invertable matrix
## and the corresponding inverse matrix if it is already been computed
## the inverse matrix is cached 

## makeCacheMatrix function creates a list with the following functions:
##    set - sets the source matrix
##    get - gets the previousle stored source matrix
##    setinverse - sets the inverse matrix
##    getinverse - gets the inverse matrix
##
## arguments:
##      invertable matrix
##
## sample usage: m1<-makeCacheMatrix(matrix(1:4,2,2))
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) m <<- inv
        
        getinv <- function() m
        
        list(set = set, get = get,
                setinv = setinv, getinv = getinv)
}


## function:    cacheSolve 
##      Function returns the inverse for invertable matrix
##      the function checks for inverse matrix before solving
##      if the inverse exits it is returned, otherwise it is computed
##      and cached before been returned
##
## arguments
##      A list object returned previously by makeCacheMatrix
##
## sample usage:
##      cacheSolve(m1)
##      
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        a <- x$get()
        m <- solve(a, ...)
        x$setinv(m)
        m
        
}
