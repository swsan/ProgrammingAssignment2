## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will return a list containing 4 functions, set(), get(), setinverse() and getinverse()
## in function space, it holds matrix x and inverse inv_x
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinverse <- function(y) inv_x <<- y
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve will return the inverse matrix inv_x and alse set the inv_x held in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  
  m <- x$get()
  inv_m <- solve(m)
  x$setinverse(inv_m)
  inv_m 
}

###testing 
a <- matrix(c(1,3,2,5,4,3,3,4,5),nrow=3,ncol=3)
x <- makeCacheMatrix(a)
x$get()
x$getinverse()
cacheSolve(x)
b1 <- x$get()
b2 <- x$getinverse()
b1
b2
b1 %*% b2
b2 %*% b1

