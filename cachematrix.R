## This functions are here to do a faster calculation of an matrix inverse
## In order to do this, they both use R lexical scooping.

## First we define an special type of matrix, this matris is a list that contain the information of the
## Matrix and the information of the inverse, if the inverse has been calculated. Also this list includes
## rutines to set/get the matrix and set/get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calcultes the inverse of our special matrix, defined via makeCacheMatrix
## Using the <<- operator this function uses the setinv routine defined in the special matrix
## to save the value of the inverse. If the inverse has already been calculated, it checks the value
## and saves time by returning it without calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

