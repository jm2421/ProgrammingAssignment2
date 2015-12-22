## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function is modeled closely after the example function
## in the readme file. It when the set/creator function is called,
## it assigns the value supplied to set as the value x, and assigns
## NULL to a variable xi (the placeholder for the inverse functions)
## it also creates the same helper functions - get, setinverse and
## getinverse

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xi <<- inverse
  getinverse <- function() xi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## again, this is closely modeled on the example
## xi is assigned the current value via the getinverse
## helper function (which is either NULL if it hasn't been
## set yet, or the actual inverse, in which case a message
## is printed). Otherwise, the solve function is called on x,
## the result is assigned to xi, and the setinverse function is
## called with xi as an input to modify x, and xi is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached inverse")
    return(xi)
  }
  data <- x$get()
  xi <- solve(data)
  x$setinverse(xi)
  xi
}

## simple test to confirm correct operation
a <- rbind(c(2., 3.),c(2., 2.))
x <- makeCacheMatrix(a)
print(x$get())
#First time - without cache
xi <- cacheSolve(x)
print(xi)
#Second time - with cache
xi <- cacheSolve(x)
print(xi)

yi <- x$getinverse()
print(yi)

