

cachematrix<- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cachesolvve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
       message("data cache")
       return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}