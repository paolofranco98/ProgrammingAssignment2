## The two functions allow me to create a matrix
## cache its inverse and compute it, avoiding
## time-consuming operations

## This function creates a matrix caching its inverse

makeCacheMatrix<-function(x=matrix()) {
    inversed<-NULL
    set<-function(y) {
        x<<-y
        inversed<<-NULL
    }
    get<-function() {x}
    setinverse<-function(inverse) {inversed<<-inverse}
    getinverse<-function() {inversed}
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function gives back the inverse of the matrix
## created above

cacheSolve<-function(x,...) {
    inversed<-x$getinverse()
    if(!is.null(inversed)) {
        message("ok cache data")
        return(inversed)
    }
    mat<-x$get()
    inversed<-solve(mat,...)
    x$setinverse(inversed)
    inversed
}
