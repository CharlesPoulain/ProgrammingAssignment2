## ----- Programming Assignment 2 -----
## instructions:
## source("cachematrix.R") when correct dir is set
## to use the list of makeCacheMatrix, use f<-makeCacheMatrix()
## you can use the functions in mCM as follow:
## f$get(x) f$get() f$setinv f$getinv()


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
    if(!is.null(invx)){
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    
    x$setinv(invx)
    invx
}
