##makeCacheMatrixDescription
## 1. initialize inverse matrix variable to an empty matrix that has
## the same number of rows and columns as the argument matrix.

## 2. returns a list of functions
## - reset()  restests the cached inverse matrix
## - getmatrix() returns the argument matrix passed to makeCacheMatrix
## - setcahceim()  sets the argument matrix as the cached matrix
## - getimmatrix() returns the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  im <- matrix(nrow=nrow(x),ncol=ncol(x))
  
  reset <- function(y) {
    x <<- y
    im <<- im <- matrix(nrow=nrow(x),ncol=ncol(x))
  }
  
  getmatrix <- function() x
  
  setcacheim <- function(imtoCache) im <<- imtoCache
  
  getimmatrix <- function() im
  
  list( reset = reset,
       getmatrix = getmatrix,
       setcacheim = setcacheim,
       getimmatrix=getimmatrix
  )
}


##cacheSolve Desctiption
##1. get inverse matrix from makeCacheMatrix.  It will either be nxn matrix with NA
## if the inverse hasn't been solved yet.  Or it will be the Inverse Matrix if the solve
## has already been performed.

##2
## if the inverse matrix is empty then calculate the inverse.  if the inverse matrix is
## not empty then return the cached version
## an empty matrix passed to makeCacheMatrix never gets cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getimmatrix()
  
  if(!is.na(im[1,1])) {
    message("getting cached data")
    return(im)
  }
  
  
  data<-x$getmatrix()  
  im<-solve(data)
  x$setcacheim(im)
  im
}
