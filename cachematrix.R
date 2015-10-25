#Function calculates inverse of the matrix using Solve function and also caches the output 
#of the result

# This function creates a matrix, set the value of matrix , fetch it using get command and also do the same thing 
# for inverse matrix by setinverse and getinverse function
makeCacheMatrix <- function(x = matrix())
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function()m
  list(set = set , get = get , setinverse = setinverse , getinverse = getinverse)
}

#The following function calculates the inverse of the matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the setinverse function
cacheSolve <- function(x,...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
