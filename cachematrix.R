## The function returns inverse of a given matrix, but
##  first it checks to see if the inverse was previously calculated.

## function1 MakeCacheMatrix creates a list functions - get matrix, set matrix, get matrix inversion, 
##set matrix inversion and invert matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  setmatrix<-function (y){
    x<<-y
    inv<<-NULL 
    }
  getmatrix<-function() x
  invertmatrix<-function(solve) inv<<-solve
  getinverse<-function () inv
  list(setmatrix=set, setmatrix=getmatrix,
       invertmatrix= getmatrix
       getinverse=getinvert)
}


## apply matrix inversion, but if inverse if already calculated previously, return
## the cached inverse. If no cache, calculate inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv<- x$getinverse()
  if (!is.null(inv)){
    mesage ("getting cached inverse")
    return (inv)
  }
  data<-x$(getinverse)
  inv<-inverse(data,...)
  x$invertmatrix
  inv
}
