## Stephen Goldberger
## 7/26/2014

## In my own words as far as I can understand:

## The first function creates a list of functions useful for inverting a matrix.
## The four functions:
## Set - Set the matrix we want to invert
## Get - Return the matrix we want to invert
## setInv - Set the matrix inverse
## getInv - Return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        INV <- NULL
        set <- function (y) {
                x <<- y
                INV <<- NULL
        }
        get <- function() x
        setINV <- function(solve) INV <<- solve
        getINV <- function() INV
        list (set = set, get = get, 
                setINV = setINV,
                getINV = getINV)
        
}

## The Second function takes this list of functions and checks to see if 
## the inverse has already been defined or not.  If it has it then it 
## returns that inverse.

## If it has not, then it solves for the Inverse, while simultaneously setting
## the inv in the makeCacheMatrix list.  So the makeCacheMatrix list which
## is sent to the cacheSolve function will have it's inverse solved for
## and if it gets sent to cacheSolve again it will return this cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INV <- x$getINV()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        data <- x$get()
        INV <- solve(data, ...)
        x$setINV(INV)
        INV
}
