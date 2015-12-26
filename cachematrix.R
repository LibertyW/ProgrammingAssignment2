## This pair of functions can together cache the inverse of a matrix.

## This function creates a matrix x which can cache the inverse matrix of itself.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)inv<<-solve
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function gives the inverse of the matrix returned by the function makeCacheMatrix.
## It will get the cached data if the inverse has been calculated before.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
   ## Return a matrix that is the inverse of 'x'
  inv
}
