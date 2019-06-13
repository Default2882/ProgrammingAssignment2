## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##Sets the inverse variable to NULL
  i <- NULL
  set <- function(y) {
    ##Sets the matix variable x to the provided matrix y and sets the inverse to i
    x <<- y
    i <<- NULL
  }
  ##gives the matrix object stored
  get <- function() x
  setInverse <- function(inv) i <<- inv ##fucntion to store the inverse of the matrix, if the user already knows it
  getInverse <- function() i ##function to print the matrix is it is already available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##A list to use the nested funciton 
}


## : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse() ##gets the inverse which is stored and store it in the variable i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }   ##check it the inverse exists, if it does simply return the inverse else calculate it
  data <- x$get()  ##Gets the matix of which the inverse has to be calculated
  i <- solve(data, ...)  ##Calculating the inverse using solve() function
  x$setInverse(i)  ##Setting the calculated inverse
  i
}
