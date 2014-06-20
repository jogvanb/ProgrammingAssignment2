## This is a pair of functions to enable R to cache inverted matrices to save
## computation time

## this function is used to create instaces that hold the inverted matrix in m
## and can be called be the functio cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  setinverse<-function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## this function returns a matrix that is the inverse of 'x' if the inverse
## alreade exists in cache the value is returned from cache otherwise the
## inverse matrix is calculated before it is returned

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
