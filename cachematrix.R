## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix from x and gives the neccessary means to 
## store its inverse, the function mainly returns a list consisting of setters and getters for the
## given matrix and its inverse.
## Since the dimentions of the inverse of some matrix A must have the same dimentions of A,
## the function prints a warning message when the dimentions of the given inverse are different from
## the dimentions of A.
## The setters and getters are
## ...$set(x) : Where x is a matrix.
## ...$get() : returns the stored matrix
## ...$setinverse(b) sets the inverse of the matrix x (or it could just be another matrix)
## ...$getinverse() returns the matrix stored by setinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    if(!identical(dim(inverse), dim(x)))
      warning("The given inverse dimentions does not match the stored matrix dimentions.")
    inv <<-inverse
  }
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function takes an argument X and optional arguments that are passed to solve as solve(X, ...).
## Then it checks whether the inverse exists, if it does it returns the inverse if it doesn't it 
## calculates it and then stores it in the passed object.
##  ...$get() returns X ^ -1
##  ...$getinverse() returns X 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getinverse())){
    x$getinverse()
  }else{
    x$setinverse(solve(x$get()), ...) 
  }
}
