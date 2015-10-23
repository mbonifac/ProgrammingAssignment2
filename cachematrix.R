## Edited by mbonifac
## makeCacheMatrix finds the inverse of a matrix and saves the result as local variable setinv
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y  ## initialize local variable x
            m <<- NULL  ## initialize local variable m 
        }
        get <- function() x ## cache the matrix you started with
        setinv <- function(solve) m <<- solve ## will result in error in first run
        ## as there is no default defined for m since it was set to NULL
        getinv <- function() m ## as m was initlized as NULL, will remain null
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        ## generate list of variables
}

## After caching variables using b<-makeCacheMatrix(x), use cacheSolve to return the inverse
## of matrix x

cacheSolve <- function(b, ...) {
        m <- b$getinv()  ## set m to be equal to getinv from makeCacheMatrix
        ## First time this is run, m will be null,
        ## Otherwise, m is the inverse of matrix x
        if(!is.null(m)) { ## if m is not null
        ## return message that inverse of x is being retrieved 
        ## instead of computing inverse of x
            message("using cached result instead of computing")
            return(m)
        }
        mtrx <- b$get() ## if computing inverse, retrieve matrix x
        m <- solve(mtrx, ...) ## update m with inverse of matrix x
        b$setinv(m)  ## now save inverse of matrix m as setinv
        m  ## return inverse of matrix m
}
