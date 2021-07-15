makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setIn <- function(inverse) {inv <<- inverse}
  getIn <- function() {inv}
  list(set = set, get = get, setIn = setIn, getIn = getIn)
}

cacheSolve <- function(x, ...){
  inv <- x$getIn()
  if(!is.null(inv)){
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setIn(inv)
  inv
}
