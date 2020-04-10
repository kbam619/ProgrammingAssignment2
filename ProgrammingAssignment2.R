###############
# Example 1
###############

makeVector <- function(x = numberic()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmean <- function(mean)m <<- mean
  getmean <- function()m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x,...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data,...)
  x$setmean(m)
  m
}

#######################
# Assignment:Caching the Inverse of a Matrix
#######################

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)k <<- inverse
  getinverse <- function()k
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Similar to mean example, we first have to create an 
## object with this one storing a matrix designed to cache  
## its inverse rather than the mean

cacheSolve <- function(x,...) {
  k <- x$getinverse()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  sol <- x$get()
  k <- solve(sol,...)
  x$solve(k)
  k
}

## The second part of the solution returns a matrix which
## is the inverse of the original matrix, a function of x,
## using the solve function as outlined in the guidelines.
## In this example, 'k' serves as the assigned variable 
## which will return the inverse.
