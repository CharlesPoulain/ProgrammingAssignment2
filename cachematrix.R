## ----- Programming Assignment 2 -----
## !instructions:
## source("cachematrix.R") when correct dir is set
## to use the list of makeCacheMatrix, use f<-makeCacheMatrix()
## you can use the functions in mCM as follow:
## f$set(x) f$get() f$setinv(matrixInverse) f$getinv()
## then set the matrix x to create the cache : f$set(x)
## then use cacheSolve(f) to return the matrix inverse

## -- makeCacheMatric function --
## allows to cache the matrix and its inverse
## by creating a list of functions to set and
## get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invx<- NULL
## cache a matrix 
    set<- function(y) {
        x<<-y
        invx<<-NULL
    }
## return the cached matrix
    get<-function() x
## put solved as invx   
    setinv<- function(solved) invx <<- solved
## return the calculated matrix inverse
    getinv<- function() invx
    
## names the functions to be used
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    invx <- x$getinv()

## check is invx does exist and returned the cached matrix inverse
    if(!is.null(invx)){
        message("getting cached data")
        return(invx)
    }
## if invx is null then call solve to get data.
    else{
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx 
    }

}
