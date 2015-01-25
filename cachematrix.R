## Set of functions to Cache the mean of a matrix and solve 
## for its inverse if no prior cache is found

## Creates a list of functions to set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #Clear prior cached variables
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  #create list of functions and environments for matrix solving
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Solves for the inverse of cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #if matrix inverse is not cached, calculate the inverse and cache it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #retrive data from makeCache list
  data <- x$get()
  #calculate inverse if not present
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
