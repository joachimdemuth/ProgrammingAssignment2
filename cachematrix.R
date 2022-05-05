
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##m set as NULL - Will take the value of the inverse matrix
  set <- function(y) { 
    x <<- y
    m <<- NULL ## if a new matrix is made, m is reset to NULL
  }
  get <- function() x ## returns the value of the matrix
  
  setInverse <- function(inverse) m <<- inverse ##assigns the value of m
  getInverse <- function() m ##gets the value of m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## for calling the functions the $
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() 
  if(!is.null(m)) { ##checks if m is not NULL. If it is not, it returns a message and the value of m
    message("Cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
