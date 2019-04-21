makeCacheMatrix <- function(x = matrix()) {
        invt <- NULL
        #setter for matrix
        set <- function(y) {
                x <<- y
                invt <<- NULL
        }
        #setter for matrix invese
        get <- function() x
        setInvert <- function(invtMatrix) invt <<- invtMatrix
        getInvert <- function() invt
        #list of functions for matrix
        list(set=set, get=get, setInvert=setInvert, getInvert=getInvert)
}

cacheSolve <- function(x, ...) {
        invt <- x$getInvert()
        #return cached inverted matrix
        if(!is.null(invt)) {
                message("inverse is cached")
                return(invt)
        }
        #compute inverse of matrix 
        m <- x$get()
        invt <- solve(m, ...)
        #cache inverse
        x$setInvert(invt)
        #return inverted matrix
        return(invt)
}