## These two functions are used together to create a matrix and find it's inverse.
## The novelty is that if the user asks for the same matrix to be inverted repeatedly,
## then a cached inverse will be return, rather than the computation being repeated.


## This function creates a list of 4 functions.  The first thing the user should
## do is set makeCacheMatrix to a variable, for using later. example: w<-makeCacheMatrix
## Afterwards, the list of functions is used like this: w$set(matrix()), w$get(), etc.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    }
  get<-function(){x}
  setinverse<-function(inverse){inv<<-inverse}
  getinverse<-function(){inv}  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## After a matrix has been created using w$set(matrix()), we can pass that matrix to cacheSolve
## in order to find it's inverse. example: cacheSolve(w).  If it is the first time the matrix
## has been inverted, R will compute it.  If not, it will return the value created from the first
## time it was computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
